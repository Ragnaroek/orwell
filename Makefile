build:
	CGO_ENABLED=1 go build -o orwell cmd/main.go

install: build
	mv ./orwell ~/bin/orwell/

run: build
	./orwell
