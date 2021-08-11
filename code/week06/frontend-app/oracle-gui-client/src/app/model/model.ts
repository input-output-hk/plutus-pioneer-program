
export interface WalletMap {
    [key: string]: string
}

export interface Wallet {
    walletName: string
    walletUuid: string
    tokens?: Token[] 
}

export interface Token {
    currencySymbol: string;
    tokenName: string;
    ammount: number;
}

export interface LoveLaceOffer {
    offeredLovelaces: number;
}