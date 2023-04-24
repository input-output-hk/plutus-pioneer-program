import MintNFTButton from "../components/MintNFTButton";
import { useContext } from "react";
import { AppStateContext } from "./_app";
import Oracle from "@/components/Oracle";
import { ExplorerLink, ExplorerLinkPrime } from "@/components/ExplorerLinks";
import DeployScripts from "@/components/DeployScripts";

export default function Home() {
    const { appState } = useContext(AppStateContext);
    const {
        wAddr,
        scPolicyIdHex,
        scAssetClassHex,
        oracleWithNftUTxO,
        oracleAddress,
        minPercent,
        txCollScriptDeployment,
        txMintingPolScriptDeployment,
    } = appState;

    return (
        <main className="flex min-h-screen flex-col items-center justify-between pt-4 px-10">
            <div className="flex flex-col items-center justify-start w-full mt-2">
                <ExplorerLink
                    message="Wallet: "
                    type="address"
                    value={wAddr || ""}
                />
                <ExplorerLink
                    message="Oracle address: "
                    type="address"
                    value={oracleAddress || ""}
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
                    link={
                        oracleWithNftUTxO?.txHash
                            ? oracleWithNftUTxO?.txHash
                            : "No TxHash"
                    }
                    value={
                        oracleWithNftUTxO?.datum
                            ? oracleWithNftUTxO?.datum
                            : "No Datum"
                    }
                />
                <ExplorerLink
                    message="Tx that deployed the Collateral script: "
                    type="tx"
                    value={txCollScriptDeployment || ""}
                />
                <ExplorerLink
                    message="Tx that deployed the Minting script: "
                    type="tx"
                    value={txMintingPolScriptDeployment || ""}
                />
                <ExplorerLink
                    message="Stablecoin PolicyId in Hex:"
                    type="policy"
                    value={scPolicyIdHex || ""}
                />
                <ExplorerLink
                    message="Stablecoin AssetClass in Hex:"
                    type="asset"
                    value={scAssetClassHex || ""}
                />

                <div>
                    <b>Depoyed with Minimum Percentage of Collateral:</b>{" "}
                    {minPercent}
                </div>
            </div>

            <div className="bg-violet-100 outline p-5">
                <MintNFTButton />
                <Oracle />
            </div>

            <div className=" bg-green-100 outline p-5">
                <DeployScripts />
            </div>
        </main>
    );
}
