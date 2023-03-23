FROM robertinoiog/plutus-pioneer-program-4:v3 as cargo

WORKDIR /app
RUN curl https://sh.rustup.rs -sSfo rustup-init
RUN chmod +x rustup-init
RUN ./rustup-init -y

FROM cargo as deno

RUN /root/.cargo/bin/cargo install deno --locked

# FROM cargo as deno

# RUN cargo install deno --locked