import * as L from "https://deno.land/x/lucid@0.9.8/mod.ts";
import { assert } from "https://deno.land/std@0.90.0/testing/asserts.ts";
import * as fc from 'https://cdn.skypack.dev/fast-check';

// the script that we are testing.
const negativeRTimedValidator: L.SpendingValidator = {
  type: "PlutusV2",
  script: "590a8f590a8c010000323322323232323322323232323232323233223232323232323232333222323232232223232533532323253355335333573466e24009200002902815335323232350022235002223500522350022253335333501600b00600215335001153350051333501500b00300710331333501500b00300710331333501500b0030073535003220022222222222220053350113350133502400402a335012502302a123333333300122333573466e1c0080040b00ac894cd4ccd5cd19b8700200102c02b101215335333573466e240080040b00ac4040404488ccd5cd19b8800200102c02b22333573466e240080040b00ac88ccd5cd19b8900200102b02c22333573466e200080040ac0b0894cd4ccd5cd19b8900200102c02b10011002225335333573466e240080040b00ac4008400440a44cd5ce248114646561646c696e65206e6f7420726561636865640002810281029133573892011c65787065637465642061206e656761746976652072656465656d6572000283333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4084088d5d0a80619a8108111aba1500b33502102335742a014666aa04aeb94090d5d0a804999aa812bae502435742a01066a0420586ae85401cccd540940b5d69aba150063232323333573466e1cd55cea80124000466a0446464646666ae68cdc39aab9d5002480008cd40a0cd40ddd69aba15002303a357426ae8940088c98c80f0cd5ce01e81e01d09aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a81399a81bbad35742a00460746ae84d5d1280111931901e19ab9c03d03c03a135573ca00226ea8004d5d09aba2500223263203833573807207006c26aae7940044dd50009aba1500533502175c6ae854010ccd540940a48004d5d0a801999aa812bae200135742a00460566ae84d5d1280111931901a19ab9c035034032135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860366ae84d5d1280211931901319ab9c027026024375a00a6666ae68cdc39aab9d5005480008dd69aba135573ca00c464c6404866ae70094090088408c4c98c808ccd5ce2490350543500023135573ca00226ea80044dd500089111a801111a801912999a999a8040038020010a99a80188008813081288130911191919192999a80310a999a80310a999a80410980224c26006930a999a80390980224c2600693080688058a999a80390980224c26006930a999a80310980224c260069308060a999a80290805080588048a999a80290a999a803909802a4c26008930a999a803109802a4c2600893080608050a999a803109802a4c26008930a999a802909802a4c2600893080592999a80290a999a80390a999a80390999a8058050010008b0b0b08058a999a80310a999a80310999a8050048010008b0b0b0805080492999a80210a999a80310a999a80310999a8050048010008b0b0b08050a999a80290a999a80290999a8048040010008b0b0b0804880412999a80190a999a80290a999a80290999a8048040010008b0b0b08048a999a80210a999a80210999a8040038010008b0b0b0804080392999a80110a999a80210a999a80210999a8040038010008b0b0b08040a999a80190a999a80190999a8038030010008b0b0b08038803091a800911111110038911001891100109110008910919800801801091091980080180109109198008018010919118011bac001320013550182233335573e0024a014466a01260086ae84008c00cd5d100100b119191999ab9a3370e6aae7540092000233221233001003002300c35742a004600a6ae84d5d1280111931900b19ab9c017016014135573ca00226ea80048c8c8c8c8cccd5cd19b8735573aa00890001199991110919998008028020018011919191999ab9a3370e6aae7540092000233221233001003002301535742a00466a01e0286ae84d5d1280111931900d99ab9c01c01b019135573ca00226ea8004d5d0a802199aa8043ae500735742a0066464646666ae68cdc3a800a4008464244460040086ae84d55cf280191999ab9a3370ea0049001119091118008021bae357426aae7940108cccd5cd19b875003480008488800c8c98c8074cd5ce00f00e80d80d00c89aab9d5001137540026ae854008cd402dd71aba135744a004464c6402e66ae7006005c0544d5d1280089aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa02a44646666aae7c008940208cd401ccc8848cc00400c008c018d55cea80118029aab9e500230043574400602826ae840044488008488488cc00401000c488c8c8cccd5cd19b875001480008c8488c00800cc014d5d09aab9e500323333573466e1d40092002212200123263201233573802602402001e26aae7540044dd5000919191999ab9a3370ea002900311909111180200298039aba135573ca00646666ae68cdc3a8012400846424444600400a60126ae84d55cf280211999ab9a3370ea006900111909111180080298039aba135573ca00a46666ae68cdc3a8022400046424444600600a6eb8d5d09aab9e500623263201233573802602402001e01c01a26aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263200e33573801e01c01826aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931900619ab9c00d00c00a13754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931900a99ab9c01601501301201101000f00e00d135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98c8038cd5ce00780700600589aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6401666ae7003002c0240204d55cea80089baa00112232323333573466e1d400520042500623333573466e1d400920022350083006357426aae7940108cccd5cd19b87500348000848880088c98c8030cd5ce00680600500480409aab9d5001137540022424446006008224440024646666ae68cdc3a800a4004401046666ae68cdc3a801240004010464c6400c66ae7001c01801000c4d55ce9baa0014984800524010350543100122002122001112323001001223300330020020011",
};
const negativeRTimedAddr: L.Address = await (await L.Lucid.new(undefined, "Custom")).utils.validatorToAddress(negativeRTimedValidator);

// the typed datum and redeemer that we are going to use.
const NegativeRTimedDatum = L.Data.Object({
  deadline: L.Data.Integer()
});
type NegativeRTimedDatum = L.Data.Static<typeof NegativeRTimedDatum>;

const NegativeRTimedRedeemer = L.Data.Integer();
type NegativeRTimedRedeemer = L.Data.Static<typeof NegativeRTimedRedeemer>;

// the function that given the context of lucid, the wallet,the datum and sends 10 ada to the script address.
async function sendToScript(
    lucid: L.Lucid,
    userPrivKey: L.PrivateKey,
    dtm: NegativeRTimedDatum
  ): Promise<L.TxHash> {
  lucid.selectWalletFromPrivateKey(userPrivKey);
  const tx = await lucid
    .newTx()
    .payToContract(negativeRTimedAddr, { inline: L.Data.to<NegativeRTimedDatum>(dtm,NegativeRTimedDatum) }, { lovelace: 10000000n })
    .complete();
  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();
  return txHash
}

// the function that given the context of lucid and a negative redeemer, grabs funds from the script address.
async function grabFunds(
    lucid: L.Lucid,
    emulator: L.Emulator,
    userPrivKey: L.PrivateKey,
    dtm: NegativeRTimedDatum,
    r: NegativeRTimedRedeemer
  ): Promise<L.TxHash> {
  lucid.selectWalletFromPrivateKey(userPrivKey);
  const rdm: L.Redeemer = L.Data.to<NegativeRTimedRedeemer>(r,NegativeRTimedRedeemer);
  const utxoAtScript: L.UTxO[] = await lucid.utxosAt(negativeRTimedAddr);
  const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.datum == L.Data.to<NegativeRTimedDatum>(dtm,NegativeRTimedDatum));
  
  if (ourUTxO && ourUTxO.length > 0) {
      const tx = await lucid
          .newTx()
          .collectFrom(ourUTxO, rdm)
          .attachSpendingValidator(negativeRTimedValidator)
          .validFrom(emulator.now())
          .complete();

      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
      return txHash
  }
  else throw new Error("UTxO's Expected!")
}

async function runTest(dtm: NegativeRTimedDatum, r: NegativeRTimedRedeemer, n: number) {
  // setup a new privateKey that we can use for testing.
  const user1: L.PrivateKey = L.generatePrivateKey();
  const address1: string = await (await L.Lucid.new(undefined, "Custom")).selectWalletFromPrivateKey(user1).wallet.address();

  const user2: L.PrivateKey = L.generatePrivateKey();
  const address2: string = await (await L.Lucid.new(undefined, "Custom")).selectWalletFromPrivateKey(user2).wallet.address();

  // Setup the emulator and give our testing wallet 10000 ada. These funds get added to the genesis block.
  const emulator = new L.Emulator([{ address: address1, assets: { lovelace: 10000000000n } }, { address: address2, assets: { lovelace: 10000000000n}}]);
  const lucid = await L.Lucid.new(emulator);

  // gets added to the first block in the emulator
  await sendToScript(lucid,user1,dtm);

  // wait n slots
  emulator.awaitSlot(n);

  // gets added to the (n+1)th block
  await grabFunds(lucid,emulator,user2,dtm,r);

  emulator.awaitBlock(10);

  //console.log(await emulator.getUtxos(address2));
}
//await runTest({deadline:BigInt(Date.now()+20000*5+1000)},-42n,5*20+1);

// UNIT tests

function testSucceed(
  str: string, // the string to display of the test
  r: bigint,   // the redeemer number
  d: bigint,   // the deadline in seconds from now
  n:number     // the number of slots user 2 waits
) {
  Deno.test(str, async () => {await runTest({deadline:BigInt(Date.now())+d},r,n)})
}

async function testFails(
  str: string, // the string to display of the test
  r: bigint,   // the redeemer number
  d: bigint,   // the deadline in seconds from now
  n:number     // the number of slots user 2 waits
) {
  Deno.test(str,async () => {
    let errorThrown = false;
    try {
      await runTest({deadline:BigInt(Date.now())+d},r,n);
    } catch (error) {
      errorThrown = true;
    }
    assert(
      errorThrown,
      "Expected to throw an error, but it completed successfully"
    );
  });
};

// deadline is slot 100 and user 2 claims at slot 120
testSucceed("UT: User 1 locks and user 2 takes with R = -42 after dealine; succeeds",-42n,BigInt(1000*100),120);
// deadline is slot 100 and user 2 claims at slot 120
testSucceed("UT: User 1 locks and user 2 takes with R = 0 after dealine; succeeds",0n,BigInt(1000*100),120);
// deadline is slot 100 and user 2 claims at slot 120
testFails("UT: User 1 locks and user 2 takes with R = 42 after dealine; fails",42n,BigInt(1000*100),120);
// deadline is slot 100 and user 2 claims at slot 80
testFails("UT: User 1 locks and user 2 takes with R = -42 before dealine; fails",-42n,BigInt(1000*100),80);
// deadline is slot 100 and user 2 claims at slot 80
testFails("UT: User 1 locks and user 2 takes with R = 0 before dealine; fails",-0n,BigInt(1000*100),80);
// deadline is slot 100 and user 2 claims at slot 80
testFails("UT: User 1 locks and user 2 takes with R = 42 before dealine; fails",42n,BigInt(1000*100),80);

// Property test
// set up a fixed deadline at slot 100
const dl: number = 100*1000;
// create only random 256 bit negative big integers for r.
const negativeBigIntArbitrary = fc.bigIntN(256).filter((n:bigint) => n <= 0n);
// create only random 256 bit positive big integers for r.
const positiveBigIntArbitrary = fc.bigIntN(256).filter((n:bigint) => n > 0n); 
// create only random integers that represent claiming after the deadline
const afterDeadlineWaits = fc.integer().filter((n: number) => n >= dl);
// create only random integers that represent claiming before the deadline
const beforeDeadlineWaits = fc.integer().filter((n: number) => n < dl);

Deno.test("PT: Negative redeemer after deadline always succeeds", () => {
  fc.assert(fc.asyncProperty(
    negativeBigIntArbitrary, afterDeadlineWaits, async (r: bigint,n: number) => {
      try {
        await runTest({deadline:BigInt(Date.now()+dl)},r,n);
      } catch (error) {
        console.error('Test failed for r= '+ r +' with error: ' + error.message);
        throw error
      };
    }
  ),{numRuns: 100});
});

Deno.test("PT: Positive redeemer after deadline always fails", () => {
  fc.assert(fc.asyncProperty(
    positiveBigIntArbitrary, afterDeadlineWaits,async (r:bigint, n: number) => {
      let errorThrown = false;
      try {
        await runTest({deadline:BigInt(Date.now()+dl)},r,n);
      } catch (error) {
        errorThrown = true;
      }
      assert(errorThrown,'Test failed for r= ' + r + ' and n= '+ n);      
    }
  ),{numRuns:100});
})

Deno.test("PT: Anything before the deadline always fails", () => {
  fc.assert(fc.asyncProperty(
    fc.bigIntN(256), beforeDeadlineWaits,async (r:bigint, n: number) => {
      let errorThrown = false;
      try {
        await runTest({deadline:BigInt(Date.now()+dl)},r,n);
      } catch (error) {
        errorThrown = true;
      }
      assert(errorThrown,'Test failed for r= ' + r + ' and n= ' + n);      
    }
  ),{numRuns:100});
})
