FROM golang:1.16  as builder
# Git is required for fetching the dependencies.
# Ca-certificates is required to call HTTPS endpoints.
RUN apt-get update -y && apt-get install git -y ca-certificates && update-ca-certificates -y
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
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -ldflags="-w -s" -o /app/frontent-app/oracle-swap-demo

RUN chmod +x /app/frontent-app/oracle-swap-demo
COPY ./static /app/frontent-app/static

USER appuser:appuser
ENTRYPOINT ["/app/frontent-app/oracle-swap-demo"]