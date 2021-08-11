package model

type WalletStatus struct {
	CicCurrentState CicCurrentState `json:"cicCurrentState"`
}

type CicCurrentState struct {
	ObservableState ObservableState `json:"observableState"`
}

type ObservableState struct {
	GetValue [][]interface{} `json:"getValue"`
}

type UnCurrencySymbol struct {
	UnCurrencySymbol string `json:"unCurrencySymbol"`
}

type UnTokenName struct {
	UnTokenName string `json:"unTokenName"`
}
