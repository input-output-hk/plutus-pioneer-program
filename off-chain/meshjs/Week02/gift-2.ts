
/*
Off-chain code for the always true validator (mkGiftValidator) defined in 
https://github.com/input-output-hk/plutus-pioneer-program/blob/plinth-plutusV3/src/Week02/Validators.hs

This code use a transaction builder when constructing the claiming transaction.
That gives a user more control over the transaction. 

The claimFunds() function is constructed in similar way as in the following MeshJS code:
https://github.com/MeshJS/examples/blob/main/aiken-giftcard/src/redeem.ts and 
https://github.com/MeshJS/examples/blob/main/aiken-vesting/src/withdraw-fund.ts

NOTES: Uncomment the function calls at the end. Run each call separetly. 
       If a function call requires input data as a transaction hash provide it. 
       Before running the code input a blockfrost key for preview in line 37. 
*/

import { 
  BlockfrostProvider, 
  MeshWallet, 
  Transaction, 
  PlutusScript,
  resolvePlutusScriptAddress,
  MeshTxBuilder,
  deserializeAddress,
  SLOT_CONFIG_NETWORK,
  unixTimeToEnclosingSlot,
  applyCborEncoding
} from "@meshsdk/core";
import { UTxO } from "@meshsdk/common";
import { CardanoSDKSerializer } from "@meshsdk/core-cst";
import { secretSeed } from "./seed.ts";
/* seed.ts has to be in form of: 
   export const secretSeed = ["seed1", "seed2", ... ] */

// Define blockchain provider and wallet 
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

// Define wallet address and public key hash of it 
const walletAddress = await wallet.getChangeAddress();
const signerHash = deserializeAddress(walletAddress).pubKeyHash;

// Defining our gift script 
const giftScript: PlutusScript = {
  code: applyCborEncoding("450101002499"),
  version: "V3"
};
const scriptAddr = resolvePlutusScriptAddress(giftScript, 0);

// Function for creating UTXO at gift script 
async function sendFunds(amount: string) {
  const tx = new Transaction({ initiator: wallet })
    .setNetwork("preview")
    .sendLovelace({address: scriptAddr, datum: {value: "", inline: true }}, amount)
    .setChangeAddress(walletAddress);

  const txUnsigned = await tx.build();
  const txSigned = await wallet.signTx(txUnsigned);
  const txHash = await wallet.submitTx(txSigned);
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

// Define the transaction builder 
const txBuilder = new MeshTxBuilder({
  fetcher: provider, // get a provider https://meshjs.dev/providers
  submitter: provider,
  evaluator: provider,
  serializer: new CardanoSDKSerializer(),
  /*verbose: true*/
});

// Function for claiming funds 
async function claimFunds(txHashGiftUtxo) {
  const utxos = await wallet.getUtxos();
  const assetUtxo: UTxO = await getUtxo(scriptAddr, txHashGiftUtxo);
  const collateral = await wallet.getCollateral();
  /* Not needed for this code
  const invalidBefore = unixTimeToEnclosingSlot(
      Date.now() - 15000,
      SLOT_CONFIG_NETWORK.preview
    ) + 1; */

  const unsignedTx = await txBuilder
  .setNetwork("preview")
  .spendingPlutusScript("V3")
  .txIn(
    assetUtxo.input.txHash,
    assetUtxo.input.outputIndex,
    assetUtxo.output.amount,
    assetUtxo.output.address
  )  
  .spendingReferenceTxInInlineDatumPresent()
  .spendingReferenceTxInRedeemerValue("")
  .txInScript(giftScript.code)
  //.txOut(walletAddress, []) // is not needed
  .txInCollateral(
    collateral[0].input.txHash,
    collateral[0].input.outputIndex,
    collateral[0].output.amount,
    collateral[0].output.address
  )
  //.invalidBefore(invalidBefore) // is not needed
  .requiredSignerHash(signerHash)
  .changeAddress(walletAddress)
  .selectUtxosFrom(utxos)
  .complete();

  const signedTx = await wallet.signTx(unsignedTx);
  const txHash = await wallet.submitTx(signedTx);
  return txHash
}

// Function calls 
// ------------------------------------------------------------------
//console.log(await sendFunds("5000000"));
// NOTE: Input the correct transaction hash that sendFunds() returns
//console.log(await claimFunds("<tx-hash>"));
