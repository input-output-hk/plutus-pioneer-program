FROM robertinoiog/plutus-pioneer-program-4:v4 as testing-docs

RUN /root/.cargo/bin/cargo install mdbook
RUN echo "alias serve-testing-docs='mdbook serve /workspace/docs/psm-docs'" >> ~/.bashrc