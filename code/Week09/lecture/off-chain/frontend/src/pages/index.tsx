import MintNFTButton from "../components/MintNFTButton";
import { useContext } from "react";
import { AppStateContext } from "./_app";
import Oracle from "@/components/Oracle";
import { ExplorerLink, ExplorerLinkPrime } from "@/components/ExplorerLinks";
import DeployScripts from "@/components/DeployScripts";
import { HiUserCircle } from "react-icons/hi";
import { IoReloadCircleSharp } from "react-icons/io5";
import Stablecoin from "@/components/Stablecoin";
import { useState } from "react";
import { Data, fromHex } from "lucid-cardano";

export default function Home() {
    type Person = "oracle" | "user" | "owner";
    const [isPerson, setIsPerson] = useState<Person>("oracle");
    const { appState, setAppState } = useContext(AppStateContext);
    const {
        wAddr,
        scPolicyIdHex,
        scAssetClassHex,
        oracleWithNftUTxO,
        oracleAddress,
        minPercent,
        txScriptsDeployment,
    } = appState;

    const refreshWallet = async () => {
        if (!appState.lucid || !window.cardano.nami) return;
        const nami = await window.cardano.nami.enable();
        appState.lucid.selectWallet(nami);
        setAppState({
            ...appState,
            wAddr: await appState.lucid.wallet.address(),
        });
    };

    const handleClick = (v: Person) => {
        if (v === "oracle") {
            setIsPerson("oracle");
        } else if (v === "user") {
            setIsPerson("user");
        } else {
            setIsPerson("owner");
        }
        console.log(isPerson);
    };

    return (
        <main className="flex min-h-screen w-screen h-screen gap-6 flex-row-reverse items-center justify-between px-5 pb-5  pt-20 bg-zinc-800">
            <div className="flex flex-col items-center justify-start  w-[380px] mt-2">
                {/* USER LOGGED */}
                <div className="absolute justify-center items-center right-0 top-5 bg-zinc-50  h-12  w-48 rounded-l-2xl flex flex-row">
                    <HiUserCircle
                        className="text-4xl text-zinc-600"
                        onClick={refreshWallet}
                    />
                    <p className="text-lg mx-2 text-zinc-800">
                        {wAddr ? `...${wAddr.substring(102)}` : ""}
                    </p>
                    <IoReloadCircleSharp
                        className="text-3xl mx-2 text-zinc-600 active:text-zinc-800"
                        onClick={refreshWallet}
                    />
                </div>

                {/* INFORMATION TABLE */}
                <p className=" overflow-clip self-start tracking-[0.2em]  text-xs text-zinc-200">
                    INFO TABLE
                </p>

                <div className=" overflow-hidden bg-zinc-50 rounded-lg w-full my-4 h-auto border border-spacing-1 border-zinc-50">
                    <ExplorerLink
                        message="Wallet: "
                        type="address"
                        value={
                            wAddr
                                ? `${wAddr.substring(
                                      0,
                                      15
                                  )}...${wAddr.substring(100)}`
                                : ""
                        }
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
                        type="tx"
                        link={
                            oracleWithNftUTxO?.txHash
                                ? oracleWithNftUTxO?.txHash
                                : "No TxHash"
                        }
                        value={
                            oracleWithNftUTxO?.datum
                                ? Data.from(oracleWithNftUTxO?.datum).toString()
                                : ""
                        }
                    />
                    <ExplorerLink
                        message="Tx that deployed the reference scripts: "
                        type="tx"
                        value={txScriptsDeployment || ""}
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

                    <div className="font-quicksand h-16 w-full overflow-hidden">
                        <p className=" bg-zinc-800  text-base text-zinc-100 h-8 pt-[6px] pl-2">
                            Depoyed with Minimum Locked % of:
                        </p>{" "}
                        {minPercent}
                    </div>
                </div>
            </div>

            {/* PERSON BUTTONS */}
            <div className="absolute top-4 left-5 flex flex-row gap-4">
                <button
                    onClick={() => handleClick("oracle")}
                    className={`${
                        isPerson == "oracle"
                            ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                            : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
                    } font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
                >
                    Oracle
                </button>
                <button
                    onClick={() => handleClick("owner")}
                    className={`${
                        isPerson == "owner"
                            ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                            : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
                    }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
                >
                    Owner
                </button>
                <button
                    onClick={() => handleClick("user")}
                    className={`${
                        isPerson == "user"
                            ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                            : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
                    }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
                >
                    User
                </button>
            </div>

            {/* ACTIONS SECTION */}
            <div className="flex flex-col items-center gap-8  h-full py-10 bg-zinc-50 w-4/5 rounded-2xl">
                {/* ORACLE ACTIONS */}
                {isPerson == "oracle" && (
                    <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9">
                        <MintNFTButton />
                        <Oracle />
                    </div>
                )}

                {/* OWNER ACTIONS */}
                {isPerson == "owner" && (
                    <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl px-9 pb-9">
                        <DeployScripts />
                    </div>
                )}

                {/* USER ACTIONS */}
                {isPerson == "user" && <Stablecoin />}
            </div>
        </main>
    );
}
