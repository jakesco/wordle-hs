FROM haskell:8.10 as build-env

WORKDIR /opt/app

RUN cabal update

COPY ./wordle-hs.cabal /opt/app/wordle-hs.cabal

RUN cabal build --only-dependencies -j4

COPY . /opt/app/
RUN cabal install --install-method=copy --installdir=.

# TODO: Look into building app in a musl environment.
# This will allow use of alpine for the prod image.
# Alpine ~10MB vs Debian ~80MB

# Production Image
FROM debian:11-slim

WORKDIR /run

COPY --from=build-env /opt/app/wordlehs .

CMD ["/run/wordlehs"]
