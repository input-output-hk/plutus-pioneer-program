package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"

	"github.com/toky03/oracle-swap-demo/model"
	"github.com/toky03/oracle-swap-demo/service"

	"github.com/gorilla/handlers"
	"github.com/gorilla/mux"
)

type oracleService interface {
	ReadWallets() ([]model.Wallet, error)

	OfferLovelaces(walletId, lovelaces string) error

	Retreive(walletId string) error

	Use(walletId string) error

	ReadFunds(walletId string) (model.Wallet, error)
}

func main() {
	handler := &handler{
		oracleService: service.CreateOracleService(),
	}
	r := mux.NewRouter()
	api := r.PathPrefix("/api").Subrouter()
	fileServer := http.FileServer(http.Dir("./static"))
	r.PathPrefix("/").Handler(http.StripPrefix("/", fileServer))
	api.HandleFunc("/wallets", handler.ReadWalletsHandler).Methods("GET")
	api.HandleFunc("/{walletId}/funds", handler.ReadFundsHandler).Methods("GET")
	api.HandleFunc("/{walletId}/offer", handler.OfferHandler).Methods("POST")
	api.HandleFunc("/{walletId}/use", handler.UseHandler).Methods("POST")
	api.HandleFunc("/{walletId}/retrieve", handler.RetrieveHandler).Methods("POST")
	api.HandleFunc("/{walletId}", handler.OptionsHandler).Methods("OPTIONS")
	headersOk := handlers.AllowedHeaders([]string{"Content-Type"})
	originsOk := handlers.AllowedOrigins([]string{"*"})
	methodsOk := handlers.AllowedMethods([]string{"GET", "HEAD", "POST", "OPTIONS", "PUT"})
	log.Fatal(http.ListenAndServe(":3001", handlers.CORS(headersOk, originsOk, methodsOk)(r)))
}

type handler struct {
	oracleService oracleService
}

func (h *handler) OptionsHandler(_ http.ResponseWriter, _ *http.Request) {
	return
}

func (h *handler) ReadWalletsHandler(w http.ResponseWriter, r *http.Request) {
	wallets, err := h.oracleService.ReadWallets()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	js, marshalErr := json.Marshal(wallets)
	writeContent(w, marshalErr, js)
}

func (h *handler) ReadFundsHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	walletId := vars["walletId"]
	funds, err := h.oracleService.ReadFunds(walletId)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	js, marshalErr := json.Marshal(funds)
	writeContent(w, marshalErr, js)

}

func writeContent(w http.ResponseWriter, marshalErr error, data []byte) {
	if marshalErr != nil {
		http.Error(w, marshalErr.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Write(data)
}

func (h *handler) OfferHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	walletId := vars["walletId"]
	var lovelaces model.OfferedLovelaces
	decoderError := json.NewDecoder(r.Body).Decode(&lovelaces)
	if decoderError != nil {
		http.Error(w, decoderError.Error(), http.StatusBadRequest)
		return
	}
	err := h.oracleService.OfferLovelaces(walletId, fmt.Sprint(lovelaces.OfferedLovelaces))
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.WriteHeader(http.StatusOK)
	return
}

func (h *handler) UseHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	walletId := vars["walletId"]

	err := h.oracleService.Use(walletId)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.WriteHeader(http.StatusAccepted)
	return
}

func (h *handler) RetrieveHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	walletId := vars["walletId"]

	err := h.oracleService.Retreive(walletId)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.WriteHeader(http.StatusAccepted)
	return
}
