# Turing-Machine
Program able to simulate a Turing machine from a machine description provided in Json.

### Prerequisites

- Opam installed (Ocaml package manager)
- X server 
- Yojson (Json library for Ocaml)

### Installing

Installing Ocaml and Opam.

For Mac OS
```
brew install ocaml--with-x11 
brew install opam
```
For GNU/Linux use your distribution package manager.

example for Arch linux
```
pacman -S ocaml
pacman -S opam
```
Install json library using opam

```
opam install yojson
```

## Compiling

```
make

./ft_turing --help
```

## Example with unary sub description

```
./ft_turing unary_sub.json 1-1=
```

![Compute unary sub description](https://raw.githubusercontent.com/brhamidi/Turing-Machine/master/example_sub.png)
</br>
![Time Complexity Mode](https://raw.githubusercontent.com/brhamidi/Turing-Machine/master/turing_curve.png)
</br>

## key learning

* Turing Machine
* State Machine
* Functional programming
* Algorithm Complexity - Time Complexity

## Authors

* Matthias Srun - [Matthias-Srun](https://github.com/Matthias-Srun)
* Brahim Hamidi - [brhamidi](https://github.com/brhamidi)

## License

This project is licensed under the GPL v3 License - see the [LICENSE](LICENSE) file for details
