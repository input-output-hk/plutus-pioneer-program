import "@/styles/globals.css";
import {
    Address,
    Blockfrost,
    Lucid,
    PolicyId,
    SpendingValidator,
    UTxO,
    Unit,
} from "lucid-cardano";
import type { AppProps } from "next/app";
import {
    Dispatch,
    SetStateAction,
    createContext,
    useEffect,
    useState,
} from "react";

export type AppState = {
    lucid?: Lucid;
    addr?: Address;
    nftPolicyIdHex?: PolicyId;
    nftTokenNameHex?: string;
    nftAssetClassHex?: Unit;
    oracleScript?: SpendingValidator;
    oracleAddress?: Address;
    oracleUTxOWithNFT?: UTxO;
};

export const AppStateContext = createContext<{
    appState: AppState;
    setAppState: Dispatch<SetStateAction<AppState>>;
}>({ appState: {}, setAppState: () => {} });

export default function App({ Component, pageProps }: AppProps) {
    const [appState, setAppState] = useState<AppState>({});

    const connectLucidAndNami = async () => {
        const lucid = await Lucid.new(
            new Blockfrost(
                "https://cardano-preview.blockfrost.io/api/v0",
                "previewfz0NMrCf2gTuGYmnkzB4KfNmM3qzYBzL"
            ),
            "Preview"
        );
        if (!window.cardano.nami) {
            window.alert("Please install Nami Wallet");
            return;
        }
        const nami = await window.cardano.nami.enable();
        lucid.selectWallet(nami);
        const addr = await lucid.wallet.address();
        setAppState({ lucid: lucid, addr: addr });
    };

    useEffect(() => {
        if (appState.lucid) return;
        connectLucidAndNami();
    }, [appState]);
    return (
        <AppStateContext.Provider value={{ appState, setAppState }}>
            <Component {...pageProps} />
        </AppStateContext.Provider>
    );
}
