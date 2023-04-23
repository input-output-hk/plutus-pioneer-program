import { AppStateContext } from "@/pages/_app";
import { safeStringToBigInt, signAndSubmitTx } from "@/utilities/utilities";
import {
    PaymentKeyHash,
    SpendingValidator,
    UTxO,
    getAddressDetails,
} from "lucid-cardano";
import { applyParamsToScript, Data } from "lucid-cardano";
import { useContext, useEffect, useState } from "react";

const CollateralDatum = Data.Enum([
    // TODO
    Data.Literal("Update"),
    Data.Literal("Delete"),
]);
type CollateralDatum = Data.Static<typeof CollateralDatum>;

export default function MintStablecoin() {
    const { appState, setAppState } = useContext(AppStateContext);
    const {
        lucid,
        wAddr,
        minPercent,
        oracleUTxOWithNFT,
        scAssetClassHex,
        collatealAddr,
        collateralRefScrUTxO,
        mpRefScrUTxO,
    } = appState;
    const [amountToMint, setAmountToMint] = useState(10n);
    const [collValueToLock, setCollValueToLock] = useState(15n);
    const [count, setCount] = useState(0);

    // useEffect(() => {
    //     getCollateralRefScrUtxO();
    //     getMpRefScrUtxO();
    //     setTimeout(() => setCount(count + 1), 3e3);
    // }, [count]);

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

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////// UPDATE ORACLE ///////////////////////////////////////////

    const mintSC = async () => {
        if (
            wAddr &&
            lucid &&
            oracleUTxOWithNFT &&
            scAssetClassHex &&
            collateralRefScrUTxO &&
            mpRefScrUTxO
        ) {
            const pkh: string =
                getAddressDetails(wAddr).paymentCredential?.hash || "";

            // const collDatum: CollateralDatum = {}; //TODO:
            const collDatum: CollateralDatum = "Update"; //TODO:

            const tx = await lucid!
                .newTx()
                .readFrom([
                    oracleUTxOWithNFT,
                    collateralRefScrUTxO,
                    mpRefScrUTxO,
                ])
                .payToContract(
                    collatealAddr,
                    { inline: collDatum },
                    { lovelace: collValueToLock }
                )
                .payToAddress(wAddr, { [scAssetClassHex]: amountToMint })
                .addSignerKey(pkh)
                .complete();

            await signAndSubmitTx(tx);
        } else {
            alert("Please, deploy the oracle before updating it!");
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////// UI /////////////////////////////////////////////////

    return (
        <div className="">
            <div className="flex flex-row">
                <p>Stablecoins to mint (units):</p>
                <input
                    type="number"
                    value={Number(amountToMint)}
                    onChange={(e) => updateMintingAndCollValues(e.target.value)}
                />
            </div>
            <div className="flex flex-row">
                <p>Collateral to lock (in Lovelace):</p>
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
                onClick={mintSC}
                disabled={!lucid || !wAddr || !amountToMint || !collValueToLock}
                className="m-3 p-3 disabled:bg-slate-400 bg-violet-400"
            >
                {" "}
                Mint Stablecoins
            </button>
        </div>
    );
}
