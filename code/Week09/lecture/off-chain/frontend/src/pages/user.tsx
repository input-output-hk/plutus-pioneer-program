import { useContext } from "react";
import { AppStateContext } from "./_app";
import { Data } from "lucid-cardano";
import { ExplorerLink, ExplorerLinkPrime } from "@/components/ExplorerLinks";
import MintStablecoin from "@/components/MintStablecoin";

export default function Home() {
    const { appState } = useContext(AppStateContext);
    const { wAddr, oracleWithNftUTxO, minPercent } = appState;

    return (
        <main className="flex min-h-screen flex-col items-center justify-between pt-4 px-10">
            <div className="flex flex-col items-center justify-start w-full mt-2">
                <ExplorerLink
                    message="Wallet: "
                    type="address"
                    value={wAddr || ""}
                />
                <ExplorerLink
                    message="Oracle UTxO with NFT: "
                    type="tx"
                    value={
                        oracleWithNftUTxO?.txHash
                            ? `${oracleWithNftUTxO?.txHash}#${oracleWithNftUTxO?.outputIndex}`
                            : ""
                    }
                />
                <ExplorerLinkPrime
                    message="Oracle's Datum (Price of ADA in cents): "
                    type="datum"
                    link={oracleWithNftUTxO?.txHash || ""}
                    value={
                        oracleWithNftUTxO?.datum
                            ? Data.from(oracleWithNftUTxO.datum)
                            : ""
                    }
                />
                <div>
                    <b>Depoyed with Minimum Percentage of Collateral:</b>{" "}
                    {minPercent}
                </div>
            </div>

            <div className="bg-violet-100 outline p-5">
                <MintStablecoin />
            </div>
        </main>
    );
}
