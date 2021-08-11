package adapter

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/toky03/oracle-swap-demo/model"
)

const (
	restEndpoint = "/api/new/contract/instance"
)

type oracleAdapter struct {
	baseUrl string
}

func CrateAdapter() *oracleAdapter {
	url := os.Getenv("ORACLE_URL")
	port := os.Getenv("ORACLE_PORT")
	if url == "" {
		url = "http://127.0.0.1"
	}
	if port == "" {
		port = "8080"
	}
	return &oracleAdapter{
		baseUrl: url + ":" + port + restEndpoint,
	}
}

func (u *oracleAdapter) OfferLovelaces(uuid, lovelaces string) error {
	payload := strings.NewReader(lovelaces)
	return u.createEmptyPostRequest("endpoint/offer", uuid, payload)
}

func (u *oracleAdapter) Retreive(uuid string) error {
	return u.createEmptyPostRequest("endpoint/retrieve", uuid, strings.NewReader("[]"))
}

func (u *oracleAdapter) Use(uuid string) error {
	return u.createEmptyPostRequest("endpoint/use", uuid, strings.NewReader("[]"))
}

func (u *oracleAdapter) ReadFunds(uuid string) error {
	return u.createEmptyPostRequest("endpoint/funds", uuid, strings.NewReader("[]"))
}

func (u *oracleAdapter) ReadStatus(uuid string) (model.WalletStatus, error) {

	url := fmt.Sprintf("%s/%s/status", u.baseUrl, uuid)

	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return model.WalletStatus{}, err
	}
	req.Header.Add("accept", "application/json")
	res, err := http.DefaultClient.Do(req)
	if err != nil {
		return model.WalletStatus{}, err
	}

	body, err := ioutil.ReadAll(res.Body)

	if err != nil {
		return model.WalletStatus{}, err
	}
	var walletStatus model.WalletStatus
	err = json.Unmarshal(body, &walletStatus)
	res.Body.Close()
	return walletStatus, nil

}

func (u *oracleAdapter) createEmptyPostRequest(endpoint, uuid string, payload *strings.Reader) error {
	url := fmt.Sprintf("%s/%s/%s", u.baseUrl, uuid, endpoint)

	req, err := http.NewRequest("POST", url, payload)
	if err != nil {
		return err
	}
	req.Header.Add("content-type", "application/json")
	res, err := http.DefaultClient.Do(req)
	if err != nil {
		return err
	}
	if res.StatusCode != http.StatusOK {
		body, _ := ioutil.ReadAll(res.Body)
		return errors.New("Status does not match expectation of 200 actual status is: " + res.Status + " content " + string(body))
	}
	time.Sleep(1 * time.Second)
	return nil
}
