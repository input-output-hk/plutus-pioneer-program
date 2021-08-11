FROM golang:1.14 as builder
# Git is required for fetching the dependencies.
# Ca-certificates is required to call HTTPS endpoints.
RUN apt-get update && apt-get install git ca-certificates && update-ca-certificates
ENV USER=appuser
ENV UID=10001
RUN adduser \
    --disabled-password \
    --gecos "" \
    --home "/nonexistent" \
    --shell "/sbin/nologin" \
    --no-create-home \
    --uid "${UID}" \
    "${USER}"

WORKDIR $GOPATH/src/github.com/toky03/oracle-swap-demo

COPY . .

RUN GO111MODULE=on go mod download
RUN go mod verify
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -ldflags="-w -s" -o /go/bin/oracle-swap-demo


# STEP 2 build a small image
############################
FROM scratch

COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/
COPY --from=builder /etc/passwd /etc/passwd
COPY --from=builder /etc/group /etc/group

COPY --from=builder /go/bin/oracle-swap-demo /app/frontent-app/oracle-swap-demo
COPY ./static /app/frontent-app/static

USER appuser:appuser
ENTRYPOINT ["/app/frontent-app/oracle-swap-demo"]
