
/*
Off-chain code for the redeemer 42 validator (mk42ValidatorSmall) defined in 
https://github.com/input-output-hk/plutus-pioneer-program/blob/plinth-plutusV3/src/Week02/Validators.hs

This off-chain code reads the compiled validator code from a reference UTXO.

NOTES: Uncomment the function calls at the end. Run each call separetly. 
       If a function call requires input data as a transaction hash provide it. 
       Before running the code input a blockfrost key for preview in line 28.  
*/

import { 
  BlockfrostProvider, 
  MeshWallet, 
  Transaction, 
  PlutusScript,
  resolvePlutusScriptAddress,
  applyCborEncoding,
  MeshTxBuilder
} from "@meshsdk/core";
import { UTxO } from "@meshsdk/common";
import { secretSeed } from "./seed.ts";
/* seed.ts has to be in form of: 
   export const secretSeed = ["seed1", "seed2", ... ] */

// Define blockchain provider and wallet and wallet address
const provider = new BlockfrostProvider("<blockfrost-key>");
const wallet = new MeshWallet({
  networkId: 0, //0=testnet, 1=mainnet
  fetcher: provider,
  submitter: provider,
  key: {
    type: "mnemonic",
    words: secretSeed
  }
});
const walletAddress = await wallet.getChangeAddress();

// Defining our gift script 
const redeemer42Script: PlutusScript = {
  code: applyCborEncoding("581e010100255333573466e1d2054375a6ae84d5d11aab9e3754002229308b01"),
  version: "V3"
};
const redeemer42Addr = resolvePlutusScriptAddress(redeemer42Script, 0);

// Defining burn address 
const burnScript: PlutusScript = {
  code: applyCborEncoding("450101002601"),
  version: "V3"
};
const burnAddr = resolvePlutusScriptAddress(burnScript, 0);

// Function for creating UTXO at gift script 
async function sendFunds(amount: string) {
  const tx = new Transaction({ initiator: wallet })
    .setNetwork("preview")
    .sendLovelace(
      { address: redeemer42Addr }, 
      amount)
    .setChangeAddress(walletAddress);

  const txUnsigned = await tx.build();
  const txSigned = await wallet.signTx(txUnsigned);
  const txHash = await wallet.submitTx(txSigned);
  return txHash
}

// Deploy a reference script 
async function deployRefScript(lovelaceAmount) {
  const utxos = await wallet.getUtxos();
  const txBuilder = new MeshTxBuilder({
    fetcher: provider 
  });
  
  const unsignedTx = await txBuilder
    .txOut(burnAddr, [{ unit: "lovelace", quantity: lovelaceAmount }])
    .txOutReferenceScript(redeemer42Script.code, redeemer42Script.version)
    .changeAddress(walletAddress)
    .selectUtxosFrom(utxos)
    .complete();
  
  const signedTx = await wallet.signTx(unsignedTx);
  const txHash = await wallet.submitTx(signedTx);
  return txHash
}

// Returns a UTXO at a given address that contains the given transaction hash 
async function getUtxo(scriptAddress, txHash) {
  const utxos = await provider.fetchAddressUTxOs(scriptAddress);
  if (utxos.length == 0) {
    throw 'No listing found.';
  }
  let filteredUtxo = utxos.find((utxo: any) => {
    return utxo.input.txHash == txHash;
  })!;
  return filteredUtxo
}

// Function for claiming funds 
async function claimFunds(txHashAssetUtxo, txHashRefUTXO) {
  const assetUtxo: UTxO = await getUtxo(redeemer42Addr, txHashAssetUtxo);
  const refScriptUtxo: UTxO = await getUtxo(burnAddr, txHashRefUTXO);
  const redeemer = { data: BigInt(42) };
  
  const tx = new Transaction({ initiator: wallet, fetcher: provider })
    .setNetwork("preview")
    .redeemValue({ value: assetUtxo, 
                   script: refScriptUtxo,
                   datum: undefined,
                   redeemer: redeemer})
    .sendValue(walletAddress, assetUtxo)
    .setRequiredSigners([walletAddress]);

  const txUnsigned = await tx.build();
  const txSigned = await wallet.signTx(txUnsigned);
  const txHash = await wallet.submitTx(txSigned);
  return txHash
}

// Function calls 
// ------------------------------------------------------------------
//console.log(await sendFunds("3000000"));
//console.log(await deployRefScript("30000000"));
// NOTE: Input the correct transaction hashes that sendFunds() and deployRefScript() return
//console.log(await claimFunds("<tx-hash>", "<tx-hash>"));
