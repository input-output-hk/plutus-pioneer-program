import { useContext } from "react";
import { AppStateContext } from "./_app";
import { Data } from "lucid-cardano";
import { ExplorerLink, ExplorerLinkPrime } from "@/components/ExplorerLinks";
import MintStablecoin from "@/components/MintStablecoin";
import { HiUserCircle } from "react-icons/hi";
import { IoReloadCircleSharp } from "react-icons/io5";

export default function Home() {
  const { appState } = useContext(AppStateContext);
  const { wAddr, oracleWithNftUTxO, minPercent } = appState;

  return (
    <main className="flex min-h-screen w-screen h-screen gap-6 flex-row-reverse items-center justify-between p-5 bg-zinc-800">
      {/* INFORMATION TABLE  section*/}
      <div className="flex flex-col items-center justify-start  w-[380px] mt-2">
        {/* USER LOGGED */}
        <div className="absolute justify-center items-center right-0 top-5 bg-zinc-50  h-12  w-48 rounded-l-2xl flex flex-row">
          <HiUserCircle className="text-4xl text-zinc-600" />
          <p className="text-lg mx-2 text-zinc-800">User x</p>
          <IoReloadCircleSharp className="text-3xl mx-2 text-zinc-600 active:text-zinc-800" />
        </div>

        {/* INFORMATION TABLE */}
        <p className=" overflow-clip self-start tracking-[0.2em]  text-xs text-zinc-200">
          INFO TABLE
        </p>

        <div className="overflow-hidden bg-zinc-50 rounded-lg w-full my-4 h-auto border border-spacing-1 border-zinc-50">
          <ExplorerLink message="Wallet: " type="address" value={wAddr || ""} />
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
              oracleWithNftUTxO?.datum ? Data.from(oracleWithNftUTxO.datum) : ""
            }
          />
          <div className="font-quicksand h-16 w-full overflow-hidden">
            <p className=" bg-zinc-800  text-base text-zinc-100 h-8 pt-[6px] pl-2">
              Depoyed with Minimum Percentage of Collateral:
            </p>{" "}
            {minPercent}
          </div>
        </div>
      </div>

      {/* ACTIONS SECTION*/}
      <MintStablecoin />
    </main>
  );
}
