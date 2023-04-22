import MintNFTButton from "../components/MintNFTButton";
import { useContext, useState } from "react";
import { AppStateContext } from "./_app";
import Oracle from "@/components/Oracle";
import { Data } from "lucid-cardano";

export default function Home() {
    const { appState } = useContext(AppStateContext);
    const {
        addr,
        nftPolicyIdHex,
        nftTokenNameHex,
        nftAssetClassHex,
        oracleUTxOWithNFT,
        oracleAddress,
    } = appState;

    return (
        <main className="flex min-h-screen flex-col items-center justify-between pt-4 px-10">
            <div className="flex flex-col items-center justify-start w-full mt-2">
                <div>
                    <b>Wallet: </b>
                    {addr && `${addr.substring(0, 20)}...`}
                </div>
                <div>
                    <b>NFT PolicyId in Hex: </b> {nftPolicyIdHex}
                </div>
                <div>
                    <b>NFT TokenName in Hex: </b> {nftTokenNameHex}
                </div>
                <div>
                    <b>NFT AssetClass in Hex: </b> {nftAssetClassHex}
                </div>

                <p>Current Oracle address: {oracleAddress}</p>
                <p>
                    Current Oracle UTxO:{" "}
                    {`${oracleUTxOWithNFT?.txHash}#${oracleUTxOWithNFT?.outputIndex}`}
                </p>
                <p>
                    Current Oracle Value:{" "}
                    {oracleUTxOWithNFT?.datum &&
                        Data.from(oracleUTxOWithNFT.datum)}
                </p>
            </div>

            <div className="bg-violet-100 outline p-5">
                <MintNFTButton />
                <Oracle />
            </div>
        </main>
    );
}
