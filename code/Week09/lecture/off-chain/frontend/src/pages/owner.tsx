import MintNFTButton from "../components/MintNFTButton";
import { useContext } from "react";
import { AppStateContext } from "./_app";
import Oracle from "@/components/Oracle";
import { Data, toText } from "lucid-cardano";
import { ExplorerLink, ExplorerLinkPrime } from "@/components/ExplorerLinks";
import DeployScripts from "@/components/DeployScripts";

export default function Home() {
    const { appState } = useContext(AppStateContext);
    const {
        wAddr,
        nftPolicyIdHex,
        nftTokenNameHex,
        nftAssetClassHex,
        oracleUTxOWithNFT,
        oracleAddress,
        minPercent,
        txScripsDeployment,
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
                    message="NFT PolicyId in Hex:"
                    type="policy"
                    value={nftPolicyIdHex || ""}
                />
                <div>
                    <b>NFT TokenName in Hex: </b> {nftTokenNameHex}
                </div>
                <ExplorerLink
                    message="NFT AssetClass in Hex:"
                    type="asset"
                    value={nftAssetClassHex || ""}
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
                        oracleUTxOWithNFT?.txHash
                            ? `${oracleUTxOWithNFT?.txHash}#${oracleUTxOWithNFT?.outputIndex}`
                            : ""
                    }
                />
                <ExplorerLinkPrime
                    message="Oracle's Datum (Price of ADA in cents): "
                    type="datum"
                    link={oracleUTxOWithNFT?.txHash || ""}
                    value={
                        oracleUTxOWithNFT?.datum
                            ? Data.from(oracleUTxOWithNFT.datum)
                            : ""
                    }
                />
                <ExplorerLink
                    message="Tx that deployed the scripts: "
                    type="tx"
                    value={txScripsDeployment || ""}
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
