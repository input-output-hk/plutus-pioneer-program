package model

import (
	"fmt"
	"log"
	"regexp"
	"strconv"
)

type Wallet struct {
	WalletName string  `json:"walletName"`
	WalletUuid string  `json:"uuid"`
	Tokens     []Token `json:"tokens"`
}

type Token struct {
	CurrencySymbol string `json:"currencySymbol"`
	TokenName      string `json:"tokenName"`
	Ammount        int64  `json:"ammount"`
}

type OfferedLovelaces struct {
	OfferedLovelaces int64 `json:"offeredLovelaces"`
}

func FromOracleDto(walletStatus WalletStatus) []Token {
	tokens := make([]Token, 0)
	for _, state := range walletStatus.CicCurrentState.ObservableState.GetValue {
		currencySymbolRaw := fmt.Sprintf("%v", state[0])
		tokenNameRaw := fmt.Sprintf("%v", state[1])
		ammountRaw := fmt.Sprintf("%f", state[1].([]interface{})[0].([]interface{})[1])
		resultCurrencySymbol := extractText(currencySymbolRaw)
		resultTokenName := extractText(tokenNameRaw)
		tokens = append(tokens, Token{
			CurrencySymbol: resultCurrencySymbol,
			TokenName:      resultTokenName,
			Ammount:        extractAmmount(ammountRaw),
		})
	}
	return tokens
}

func extractText(raw string) string {
	currencyRegex := regexp.MustCompile(`map\[\w*:([a-zA-Z0-9_]*)\]`)
	result := currencyRegex.FindAllStringSubmatch(raw, 1)
	if result == nil || len(result) < 1 {
		return ""
	}
	return result[0][1]
}

func extractAmmount(raw string) int64 {
	val, err := strconv.ParseFloat(raw, 64)
	if err != nil {
		log.Printf("could not convert %v to int", raw)
		return 0
	}
	return int64(val)
}
