package service

import (
	"errors"
	"log"

	"github.com/toky03/oracle-swap-demo/adapter"
	"github.com/toky03/oracle-swap-demo/model"
)

type oracleAdapter interface {
	OfferLovelaces(string, string) error
	Retreive(string) error
	Use(string) error
	ReadFunds(string) error
	ReadStatus(string) (model.WalletStatus, error)
}

type oracleServiceImpl struct {
	oracleAdapter oracleAdapter
	wallets       map[string]string
}

func CreateOracleService() *oracleServiceImpl {
	adapter := adapter.CrateAdapter()
	return &oracleServiceImpl{
		oracleAdapter: adapter,
		wallets:       readWallets(),
	}
}

func (s *oracleServiceImpl) ReadWallets() ([]model.Wallet, error) {
	wallets := make([]model.Wallet, 0, len(s.wallets))
	for name, _ := range s.wallets {
		wallet, err := s.ReadFunds(name)
		if err != nil {
			log.Printf("error reading funds %s", err)
			return nil, err
		}
		wallets = append(wallets, wallet)
	}
	return wallets, nil
}

func (s *oracleServiceImpl) OfferLovelaces(walletId, lovelaces string) error {
	walletUuid := s.wallets[walletId]
	if walletUuid == "" {
		return errors.New("No wallet found for id " + walletId)
	}
	return s.oracleAdapter.OfferLovelaces(walletUuid, lovelaces)
}

func (s *oracleServiceImpl) Retreive(walletId string) error {
	walletUuid := s.wallets[walletId]
	if walletUuid == "" {
		return errors.New("No wallet found for id " + walletId)
	}
	return s.oracleAdapter.Retreive(walletUuid)
}

func (s *oracleServiceImpl) Use(walletId string) error {
	walletUuid := s.wallets[walletId]
	if walletUuid == "" {
		return errors.New("No wallet found for id " + walletId)
	}
	return s.oracleAdapter.Use(walletUuid)
}

func (s *oracleServiceImpl) ReadFunds(walletId string) (model.Wallet, error) {
	walletUuid := s.wallets[walletId]
	if walletUuid == "" {
		return model.Wallet{}, errors.New("No wallet found for id " + walletId)
	}
	err := s.oracleAdapter.ReadFunds(walletUuid)
	if err != nil {
		return model.Wallet{}, err
	}
	walletStatus, err := s.oracleAdapter.ReadStatus(walletUuid)
	return model.Wallet{
		WalletName: walletId,
		WalletUuid: walletUuid,
		Tokens:     model.FromOracleDto(walletStatus),
	}, nil
}
