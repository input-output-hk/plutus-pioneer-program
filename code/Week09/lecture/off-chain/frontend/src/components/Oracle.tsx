import { AppStateContext } from "@/pages/_app";
import { signAndSubmitTx } from "@/utilities/utilities";
import {
    PaymentKeyHash,
    SpendingValidator,
    UTxO,
    getAddressDetails,
} from "lucid-cardano";
import { applyParamsToScript, Data } from "lucid-cardano";
import { useContext, useEffect, useState } from "react";

const OracleRedeemer = Data.Enum([
    Data.Literal("Update"),
    Data.Literal("Delete"),
]);
type OracleRedeemer = Data.Static<typeof OracleRedeemer>;

export default function Oracle() {
    const { appState, setAppState } = useContext(AppStateContext);
    const {
        lucid,
        wAddr,
        nftPolicyIdHex,
        nftTokenNameHex,
        nftAssetClassHex,
        oracleWithNftUTxO,
        oracleScript,
        oracleAddress,
    } = appState;
    const [rate, setRate] = useState(100n);
    const [count, setCount] = useState(0);

    useEffect(() => {
        getOracleNftUtxO();
        setTimeout(() => setCount(count + 1), 5e3);
    }, [count]);

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////// HELPER FUNCTIONS ///////////////////////////////////////////

    const getOracleNftUtxO = async () => {
        if (lucid && wAddr && oracleAddress) {
            const oracUtxO = await lucid.utxosAt(oracleAddress).catch((err) => {
                console.log("Can't find Oracle UtxO");
            });
            if (!oracUtxO) return;
            const oracWithNftUTxO = oracUtxO.find((utxo: UTxO) => {
                return Object.keys(utxo.assets).some((key) => {
                    return key == nftAssetClassHex;
                });
            });
            if (
                oracWithNftUTxO == undefined ||
                oracWithNftUTxO == oracleWithNftUTxO
            )
                return;
            setAppState({
                ...appState,
                oracleWithNftUTxO: oracWithNftUTxO,
            });
        }
    };

    const parseRate = (r: string) => {
        const rate = BigInt(Number(r));
        if (Number.isNaN(rate)) return;
        setRate(rate);
    };

    const getFinalScript = async (
        pkh: PaymentKeyHash
    ): Promise<SpendingValidator | undefined> => {
        console.log("Deploying Oracle with Rate and AssetClass: ", {
            rate,
            nftPolicyIdHex,
            nftTokenNameHex,
        });
        if (!lucid || !nftPolicyIdHex || !nftTokenNameHex) return;

        const Params = Data.Tuple([Data.Bytes(), Data.Bytes(), Data.Bytes()]);
        type Params = Data.Static<typeof Params>;
        const oracleScript: SpendingValidator = {
            type: "PlutusV2",
            script: applyParamsToScript<Params>(
                "590bf7590bf401000032323322323322323232332232323232323232323232323232323232323232323232222223223232533532323232323253350041533533011500235006220011027133573892011a6f70657261746f72207369676e6174757265206d697373696e6700026153355335333573466e1cd4d40188800888ccc048d54cd4c05801484d4004880044c98c80b4cd5ce249146f7261636c6520696e707574206d697373696e670003222220030020014800809c098409c4cd5ce24918746f6b656e206d697373696e672066726f6d20696e70757400026153355335333573466e1cd4d40188800888ccc048d5400c888800c008005200202702610271335738920119746f6b656e206d697373696e672066726f6d206f75747075740002615335533533011500235006220011027133573892011a6f70657261746f72207369676e6174757265206d697373696e6700026153355335323253335350022222002150302130150012321533535003222222222222300d00221301700115032320013550342253350011503322135002225335333573466e3c00801c0c00bc4d40e00044c01800d400d4004840a04098409c4cd5ce248114696e76616c6964206f757470757420646174756d000261026102610261533553353013002213500122350012222350092235002222222222222333553027120012235002222253353501822350062232335005233500425335333573466e3c00800412011c5400c411c811c8cd4010811c94cd4ccd5cd19b8f002001048047150031047153350032153350022133500223350022335002233500223303b002001204a2335002204a23303b00200122204a222335004204a2225335333573466e1c01800c13413054cd4ccd5cd19b8700500204d04c1333573466e1c01000413413041304130411454cd40048411441144cd4114018014401541000284c98c80a8cd5ce2481024c660002f130284988854cd40044008884c0b126135001220023333573466e1cd55cea802a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4090094d5d0a80619a8120129aba1500b33502402635742a014666aa050eb9409cd5d0a804999aa8143ae502735742a01066a04805a6ae85401cccd540a00b9d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40e1d69aba150023039357426ae8940088c98c80fccd5ce01e02201e89aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a81c3ad35742a00460726ae84d5d1280111931901f99ab9c03c04403d135573ca00226ea8004d5d09aba2500223263203b33573807008007226aae7940044dd50009aba1500533502475c6ae854010ccd540a00a88004d5d0a801999aa8143ae200135742a00460586ae84d5d1280111931901b99ab9c03403c035135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00a60386ae84d5d1280291931901499ab9c02602e0273333573466e1d40192002212200223333573466e1d401d2000212200123263202933573804c05c04e04c6eb401ccc8848cc00400c008cd5409cdd70049bae008375c00e2054264c6404a66ae71241035054350002a135573ca00226ea80044d55ce9baa0012223232300100532001355025223350014800088d4008894cd4ccd5cd19b8f002009021020130070011300600332001355024223350014800088d4008894cd4ccd5cd19b8f00200702001f1001130060032235002222222222222533533355301312001501225335333573466e3c0380040940904d40b4004540b001084094408c8ccccccd5d20009280f1280f1280f11a80f9bad0022501e01f3200135501e221122253350011002221330050023335530071200100500400123500122350022222222222223333500d2502a2502a2502a23335530121200150112350012253355335333573466e3cd400888008d4010880080980944ccd5cd19b8735002220013500422001026025102513502e0031502d00d132123300122533500221003100100250193200135501b221122253350011350032200122133350052200230040023335530071200100500400122333573466e3c00800404804448c88c008dd6000990009aa80d111999aab9f00125018233501730043574200460066ae8800806c8c8c8cccd5cd19b8735573aa004900011991091980080180118051aba150023005357426ae8940088c98c8058cd5ce00980d80a09aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa004900011991091980080180118099aba1500233500d012357426ae8940088c98c806ccd5ce00c01000c89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403a66ae7006808806c0680644d55cea80089baa00135742a00466a012eb8d5d09aba2500223263201733573802803802a26ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355017223233335573e0044a02c466a02a66aa02e600c6aae754008c014d55cf280118021aba200301913574200224464646666ae68cdc3a800a400046a02e600a6ae84d55cf280191999ab9a3370ea00490011280b91931900a19ab9c011019012011135573aa00226ea80048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900a19ab9c01101901201101000f135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900819ab9c00d01500e135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8038cd5ce00580980609baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c805ccd5ce00a00e00a80a00980900880800789aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6402066ae700340540380344d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200d33573801402401601426aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401c66ae7002c04c03002c0284d55cea80089baa0012323333573466e1d40052002200523333573466e1d40092000200523263200a33573800e01e01000e26aae74dd5000891001091000a48103505431002326320033357389212265787065637465642065786163746c79206f6e65206f7261636c65206f7574707574000084984488008488488cc00401000c448848cc00400c00848488c00800c448800448004448c8c00400488cc00cc008008005",
                [nftPolicyIdHex, nftTokenNameHex, pkh],
                Params
            ),
        };
        return oracleScript;
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////// DEPLOY ORACLE ///////////////////////////////////////////

    const deployOracle = async () => {
        if (!lucid || !wAddr) {
            alert("Please connect account and mint NFT!");
            return;
        }
        const pkh: string =
            getAddressDetails(wAddr).paymentCredential?.hash || "";
        const oracle = await getFinalScript(pkh);
        if (!oracle || !nftAssetClassHex) {
            alert("Please mint NFT first!");
            return;
        }
        const oracleAddress = lucid!.utils.validatorToAddress(oracle);
        console.log("final oracle script: ", oracle);
        console.log("final oracle address: ", oracleAddress);
        setAppState({
            ...appState,
            oracleScript: oracle,
            oracleAddress: oracleAddress,
        });

        const tx = await lucid!
            .newTx()
            .payToContract(
                oracleAddress,
                { inline: Data.to(rate, Data.Integer()) },
                { [nftAssetClassHex]: 1n }
            )
            .addSignerKey(pkh)
            .complete();
        await signAndSubmitTx(tx);
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////// UPDATE ORACLE ///////////////////////////////////////////

    const updateOracle = async () => {
        if (
            wAddr &&
            lucid &&
            nftAssetClassHex &&
            oracleScript &&
            oracleWithNftUTxO &&
            oracleAddress
        ) {
            const pkh: string =
                getAddressDetails(wAddr).paymentCredential?.hash || "";

            const tx = await lucid!
                .newTx()
                .collectFrom(
                    [oracleWithNftUTxO], // UTXO to spend
                    Data.to<OracleRedeemer>("Update", OracleRedeemer) // Redeemer
                )
                .payToContract(
                    oracleAddress,
                    { inline: Data.to(rate, Data.Integer()) },
                    { [nftAssetClassHex]: 1n }
                )
                .attachSpendingValidator(oracleScript)
                .addSignerKey(pkh)
                .complete();

            await signAndSubmitTx(tx);
        } else {
            alert("Please, deploy the oracle before updating it!");
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////// DELETE ORACLE ///////////////////////////////////////////

    const deleteOracle = async () => {
        if (
            wAddr &&
            lucid &&
            nftAssetClassHex &&
            oracleScript &&
            oracleWithNftUTxO &&
            oracleAddress
        ) {
            const pkh: string =
                getAddressDetails(wAddr).paymentCredential?.hash || "";

            const tx = await lucid!
                .newTx()
                .collectFrom(
                    [oracleWithNftUTxO], // UTXO to spend
                    Data.to<OracleRedeemer>("Delete", OracleRedeemer) // Redeemer
                )
                .payToAddress(wAddr, { [nftAssetClassHex]: 1n })
                .attachSpendingValidator(oracleScript)
                .addSignerKey(pkh)
                .complete();

            await signAndSubmitTx(tx);
        } else {
            alert(
                "You have to deploy the oracle before being able to delete it!"
            );
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////// UI /////////////////////////////////////////////////

    return (
        <div className="w-full">
            <div className="flex flex-row w-full justify-center items-center my-8 text-lg text-zinc-800 font-quicksand ">
                <p>Current price of ADA (in USD cents):</p>
                <input
                    type="number"
                    value={Number(rate)}
                    onChange={(e) => parseRate(e.target.value)}
                    className="w-16 py-1 px-2 ml-2 border border-zinc-700 rounded"
                />
            </div>
            <div className="w-full flex flex-row gap-4">
                <button
                    onClick={deployOracle}
                    disabled={
                        !lucid ||
                        !wAddr ||
                        !nftAssetClassHex ||
                        rate === 0n ||
                        !!oracleWithNftUTxO
                    }
                    className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                    {" "}
                    Deploy Oracle
                </button>
                <button
                    onClick={updateOracle}
                    disabled={
                        !lucid ||
                        !wAddr ||
                        !nftAssetClassHex ||
                        rate === 0n ||
                        !oracleWithNftUTxO
                    }
                    className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                    {" "}
                    Update Oracle
                </button>
                <button
                    onClick={deleteOracle}
                    disabled={
                        !lucid ||
                        !wAddr ||
                        !nftAssetClassHex ||
                        rate === 0n ||
                        !oracleWithNftUTxO
                    }
                    className="w-full rounded-lg p-3 text-zinc-50 disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold bg-red-400 active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                    {" "}
                    Delete Oracle
                </button>
            </div>
        </div>
    );
}
