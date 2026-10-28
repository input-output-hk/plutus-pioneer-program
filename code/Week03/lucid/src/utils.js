import * as L from 'lucid-cardano';

/**
 * NOTE: Wallet actions
 */
export async function queryWalletDetails(wallet) {
    const pkh = await getCardanoPKH(wallet);
    const utxos = await wallet.getUtxos();
    /* DEBUG */ console.log("show details of utxos from pkh");
    /* DEBUG */ console.log(utxos);
    const balance = utxos.reduce((acc, utxo) => acc + utxo.assets.lovelace, 0n);

    return {
        pkh: pkh,
        balance: balance,
    };
}

export async function getCardanoPKH(wallet) {
    const addr = await wallet.address();
    const details = L.getAddressDetails(addr);
    /* DEBUG */ console.log("show address details");
    /* DEBUG */ console.log(details);
    return details.paymentCredential.hash;
}

/**
 * NOTE: Smart Contract actions
 */
export async function getVestingUTxOs(address, Datum) {
    const utxos = await window.lucid.utxosAt(address);
    /* DEBUG */ console.log("show detailed utxos from the contract addr");
    /* DEBUG */ console.log(utxos);
    let res = [];
    for (const utxo of utxos) {
        const datum = utxo.datum;
        if (datum) {
            try {
                const d = L.Data.from(datum, Datum);
                res.push({ utxo: utxo, datum: d });
            } catch (err) {
                console.log("error: unable to convert from CBOR object (could be a Map object)");
            }
        }
    }
    /* DEBUG */ console.log("show detailed _valid_ utxos from the contract addr", res);
    return res;
}

/**
 * NOTE: Tx actions
 */
async function submitCardanoTx(signedTx) {
    const tid = await signedTx.submit();
    /* DEBUG */ console.log("show Cardano tx submitted: " + tid);
    addTxLinkToTable("https://preview.cardanoscan.io/transaction/" + tid, tid);
}

export async function signAndSubmitCardanoTx(tx) {
    try {
        const signedTx = await tx.sign().complete();
        /* DEBUG */ console.log("show signedTx object", signedTx);
        await submitCardanoTx(signedTx);
    } catch (err) {
        alert(`Cardano transaction:\ninfo: ${err.info}\nmessage: ${err.message}`);
        throw (err);
    }
}

export async function findUTxO(refTx, contractAddress, Datum) {
    const [tid, idx] = refTx.split('#');
    const utxos = await utils.getVestingUTxOs(contractAddress, Datum);
    for (const utxo of utxos) {
        if (utxo.utxo.txHash == tid &&
            utxo.utxo.outputIndex == parseInt(idx)) {
            return utxo;
        }
    }
    return null;
}


/**
 * NOTE: DOM node actions
 */
export function removeChildren(node) {
    while (node.firstChild) {
        node.removeChild(node.lastChild);
    }
}

export function addCell(tr, content, copyEnabled = false) {
    const td = window.document.createElement('td');
    tr.appendChild(td);

    if (copyEnabled) {
        const node = window.document.createElement("p");
        td.appendChild(node);
        const uid = "uid_" + String(Math.random()).slice(2);
        node.setAttribute("id", uid);

        node.appendChild(window.document.createTextNode(content));
        node.addEventListener(
            "click",
            () => navigator.clipboard.writeText(window.document.getElementById(uid).firstChild.textContent)
        );
    } else {
        td.appendChild(window.document.createTextNode(content));
    }
}

function addTxLinkToTable(href, txIdText) {
    const txTable = window.document.getElementById("completedTxTable");

    const tr = window.document.createElement('tr');
    txTable.appendChild(tr);
    const td = window.document.createElement('td');
    tr.appendChild(td);
    const a = window.document.createElement('a');
    td.appendChild(a);

    a.setAttribute('href', href);
    a.setAttribute('target', '_blank');
    a.appendChild(window.document.createTextNode(txIdText));
}
