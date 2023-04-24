import { AppStateContext } from "@/pages/_app";
import {
    findUTxO,
    safeStringToBigInt,
    signAndSubmitTx,
} from "@/utilities/utilities";
import { getAddressDetails } from "lucid-cardano";
import { Data } from "lucid-cardano";
import { useContext, useEffect, useState } from "react";

const CollateralDatum = Data.Object({
    colMintingPolicyId: Data.Bytes(),
    colOwner: Data.Bytes(),
    colStablecoinAmount: Data.Integer(),
    colLock: Data.Enum([Data.Literal("Unlocked"), Data.Literal("Locked")]),
});
type CollateralDatum = Data.Static<typeof CollateralDatum>;

const CollateralRedeemer = Data.Enum([
    Data.Literal("Lock"),
    Data.Literal("Redeem"),
    Data.Literal("Liquidate"),
]);
type CollateralRedeemer = Data.Static<typeof CollateralRedeemer>;

const MintRedeemer = Data.Enum([
    Data.Literal("Mint"),
    Data.Literal("Burn"),
    Data.Literal("Liquidate"),
]);
type MintRedeemer = Data.Static<typeof MintRedeemer>;

export default function MintStablecoin() {
    const { appState, setAppState } = useContext(AppStateContext);
    const {
        lucid,
        wAddr,
        minPercent,
        oracleWithNftUTxO,
        oracleUtxoWithNFTRef,
        scAssetClassHex,
        scPolicyIdHex,
        collatealAddr,
        collateralRefScrUTxO,
        collateralRefScrUTxORef,
        collateralToUnlockUTxO,
        collateralToUnlockUTxORef,
        mintingPolRefScrUTxO,
        mintingPolRefScrUTxORef,
    } = appState;
    const [amountToMint, setAmountToMint] = useState(10n);
    const [amountToBurn, setAmountToBurn] = useState(10n);
    const [collValueToLock, setCollValueToLock] = useState(15n);

    ////// TESTING DATA. REMOVE BEFORE RECORDING /////
    useEffect(() => {
        if (!appState.oracleUtxoWithNFTRef) {
            setAppState({
                ...appState,
                oracleUtxoWithNFTRef:
                    "215d8d5ad71a05699d2d395f59ec722a0a5fd464f6026cac4e0ddae77c43c309#0",
                collateralRefScrUTxORef:
                    "889f1dd145c4842f4ae94cf5011f4b877e37aa44f3c768488012e86036ca17a6#0",
                mintingPolRefScrUTxORef:
                    "889f1dd145c4842f4ae94cf5011f4b877e37aa44f3c768488012e86036ca17a6#1",
                scPolicyIdHex:
                    "d164834f314baccf0de36b7898609464511e19f6d89e0e8507ea5219",
                scAssetClassHex:
                    "d164834f314baccf0de36b7898609464511e19f6d89e0e8507ea521955534450",
            });
        }
    }, [appState]);

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////// HELPER FUNCTIONS ///////////////////////////////////////////

    // TODO: Collateral doesn't update when updating stablecoin amount
    const updateMintingAndCollValues = (r: string) => {
        const scAmo = BigInt(Number(r));
        if (Number.isNaN(scAmo)) return;
        setAmountToMint(scAmo);
        if (!minPercent) return;
        const minColl = (Number(r) * minPercent) / 100;
        if (Number.isNaN(minColl)) return;
        setCollValueToLock(BigInt(minColl));
    };

    const getReferenceUTxOs = async () => {
        if (
            !lucid ||
            !oracleUtxoWithNFTRef ||
            !collateralRefScrUTxORef ||
            !mintingPolRefScrUTxORef
        )
            return;

        const oracUTxOWithNFT = await findUTxO(lucid, oracleUtxoWithNFTRef);
        const collRefScrUTxO = await findUTxO(lucid, collateralRefScrUTxORef);
        const mpRefScrUTxO = await findUTxO(lucid, mintingPolRefScrUTxORef);

        console.log("UTxOs: ", oracUTxOWithNFT, collRefScrUTxO, mpRefScrUTxO);
        setAppState({
            ...appState,
            oracleWithNftUTxO: oracUTxOWithNFT,
            collateralRefScrUTxO: collRefScrUTxO,
            mintingPolRefScrUTxO: mpRefScrUTxO,
        });
    };

    const getColateralUTxOToUnlock = async () => {
        // TODO: after minting works
        // if (
        //     !lucid ||
        //     !oracleUtxoWithNFTRef ||
        //     !collateralRefScrUTxORef ||
        //     !mintingPolRefScrUTxORef
        // )
        //     return;
        // const oracUTxOWithNFT = await findUTxO(lucid, oracleUtxoWithNFTRef);
        // const collRefScrUTxO = await findUTxO(lucid, collateralRefScrUTxORef);
        // const mpRefScrUTxO = await findUTxO(lucid, mintingPolRefScrUTxORef);
        // console.log("UTxOs: ", oracUTxOWithNFT, collRefScrUTxO, mpRefScrUTxO);
        // setAppState({
        //     ...appState,
        //     oracleWithNftUTxO: oracUTxOWithNFT,
        //     collateralRefScrUTxO: collRefScrUTxO,
        //     mintingPolRefScrUTxO: mpRefScrUTxO,
        // });
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// MINT STABLECOINS /////////////////////////////////////////

    const mintSC = async () => {
        console.log("mintSC -> appState: ", appState);
        if (
            wAddr &&
            lucid &&
            oracleWithNftUTxO &&
            scAssetClassHex &&
            collateralRefScrUTxO &&
            mintingPolRefScrUTxO &&
            scPolicyIdHex &&
            amountToMint > 0n
        ) {
            const pkh: string =
                getAddressDetails(wAddr).paymentCredential?.hash || "";

            const collDatum: CollateralDatum = {
                colMintingPolicyId: scPolicyIdHex,
                colOwner: pkh,
                colStablecoinAmount: amountToMint,
                colLock: "Locked",
            };

            const tx = await lucid!
                .newTx()
                .readFrom([
                    oracleWithNftUTxO,
                    collateralRefScrUTxO,
                    mintingPolRefScrUTxO,
                ])
                .payToContract(
                    collatealAddr,
                    {
                        inline: Data.to<CollateralDatum>(
                            collDatum,
                            CollateralDatum
                        ),
                    },
                    { lovelace: collValueToLock * 1000000n }
                )
                // FIXME: minting doesn't work.
                .mintAssets(
                    { [scAssetClassHex]: amountToMint },
                    Data.to<MintRedeemer>("Mint", MintRedeemer)
                )
                .addSignerKey(pkh)
                .complete();

            console.log("minting tx: ", tx.txComplete.to_js_value());

            await signAndSubmitTx(tx);
        } else {
            alert("Please, provide the reference scripts!");
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// BURN STABLECOINS /////////////////////////////////////////

    const burnSC = async () => {
        console.log("mintSC -> appState: ", appState);
        if (
            wAddr &&
            lucid &&
            scPolicyIdHex &&
            amountToBurn > 0n &&
            scAssetClassHex &&
            oracleWithNftUTxO &&
            collateralRefScrUTxO &&
            mintingPolRefScrUTxO &&
            collateralToUnlockUTxO
        ) {
            const pkh: string =
                getAddressDetails(wAddr).paymentCredential?.hash || "";

            const tx = await lucid!
                .newTx()
                .readFrom([
                    oracleWithNftUTxO,
                    collateralRefScrUTxO,
                    mintingPolRefScrUTxO,
                ])
                .collectFrom(
                    [collateralRefScrUTxO],
                    Data.to<CollateralRedeemer>("Redeem", CollateralRedeemer)
                )
                .mintAssets(
                    { [scAssetClassHex]: -amountToMint },
                    Data.to<MintRedeemer>("Mint", MintRedeemer)
                )
                .addSignerKey(pkh)
                .complete();

            console.log("minting tx: ", tx.txComplete.to_js_value());

            // await signAndSubmitTx(tx);
        } else {
            alert("Please, provide the reference scripts!");
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////// UI /////////////////////////////////////////////////

    return (
        <div className="">
            <div className="flex flex-row">
                <p>Oracle UTxO with NFT Ref:</p>
                <input
                    type="string"
                    value={oracleUtxoWithNFTRef || ""}
                    onChange={(e) =>
                        setAppState({
                            ...appState,
                            oracleUtxoWithNFTRef: e.target.value,
                        })
                    }
                />
            </div>
            <div className="flex flex-row">
                <p>Collateral UTxO Ref:</p>
                <input
                    type="string"
                    value={collateralRefScrUTxORef || ""}
                    onChange={(e) =>
                        setAppState({
                            ...appState,
                            collateralRefScrUTxORef: e.target.value,
                        })
                    }
                />
            </div>
            <div className="flex flex-row">
                <p>Stablecoin Minting Policy UTxO Ref:</p>
                <input
                    type="string"
                    value={mintingPolRefScrUTxORef || ""}
                    onChange={(e) =>
                        setAppState({
                            ...appState,
                            mintingPolRefScrUTxORef: e.target.value,
                        })
                    }
                />
            </div>
            <div className="flex flex-row">
                <p>Stablecoin Minting PolicyId in Hex:</p>
                <input
                    type="string"
                    value={scPolicyIdHex || ""}
                    onChange={(e) =>
                        setAppState({
                            ...appState,
                            scPolicyIdHex: e.target.value,
                        })
                    }
                />
            </div>

            <div className="flex flex-row">
                <p>Stablecoin AssetClass in Hex:</p>
                <input
                    type="string"
                    value={scAssetClassHex || ""}
                    onChange={(e) =>
                        setAppState({
                            ...appState,
                            scAssetClassHex: e.target.value,
                        })
                    }
                />
            </div>
            <div className="flex flex-row">
                <p>Stablecoins to mint (units):</p>
                <input
                    type="number"
                    value={Number(amountToMint)}
                    onChange={(e) => updateMintingAndCollValues(e.target.value)}
                />
            </div>
            <div className="flex flex-row">
                <p>Collateral to lock (in ADA):</p>
                <input
                    type="number"
                    value={Number(collValueToLock)}
                    onChange={(e) => {
                        const coll = safeStringToBigInt(e.target.value);
                        if (!coll) return;
                        setCollValueToLock(coll);
                    }}
                />
            </div>
            <button
                onClick={getReferenceUTxOs}
                disabled={!lucid || !wAddr || !amountToMint || !collValueToLock}
                className="m-3 p-3 disabled:bg-slate-400 bg-violet-400"
            >
                {" "}
                Get Reference Input/Script UTxOs
            </button>

            <button
                onClick={mintSC}
                disabled={
                    !lucid ||
                    !wAddr ||
                    !amountToMint ||
                    !collValueToLock ||
                    !oracleWithNftUTxO ||
                    !scAssetClassHex ||
                    !collateralRefScrUTxO ||
                    !mintingPolRefScrUTxO
                }
                className="m-3 p-3 disabled:bg-slate-400 bg-violet-400"
            >
                {" "}
                Mint Stablecoins
            </button>

            <div className="flex flex-row">
                <p>Collateral UTxO Ref to unlock:</p>
                <input
                    type="string"
                    value={collateralToUnlockUTxORef || ""}
                    onChange={(e) =>
                        setAppState({
                            ...appState,
                            collateralToUnlockUTxORef: e.target.value,
                        })
                    }
                />
            </div>

            <div className="flex flex-row">
                <p>Stablecoins to burn (units):</p>
                <input
                    type="number"
                    value={Number(amountToBurn)}
                    onChange={(e) => {
                        const am = safeStringToBigInt(e.target.value);
                        if (!am) return;
                        setAmountToBurn(am);
                    }}
                />
            </div>

            <button
                onClick={getColateralUTxOToUnlock}
                disabled={!lucid || !wAddr || !amountToMint || !collValueToLock}
                className="m-3 p-3 disabled:bg-slate-400 bg-violet-400"
            >
                {" "}
                Get Collateral UTxO to unlock
            </button>
            <button
                onClick={burnSC}
                disabled={
                    !lucid ||
                    !wAddr ||
                    !amountToMint ||
                    !collValueToLock ||
                    !oracleWithNftUTxO ||
                    !scAssetClassHex ||
                    !collateralRefScrUTxO ||
                    !mintingPolRefScrUTxO
                }
                className="m-3 p-3 disabled:bg-slate-400 bg-violet-400"
            >
                {" "}
                Burn Stablecoins
            </button>
        </div>
    );
}
