# Least squares (“electric”) voting system

_Disclaimer: This is not an official Google product._

[![Build Status](https://travis-ci.com/ppetr/least-squares-electric-voting.svg?branch=main)](https://travis-ci.com/ppetr/least-squares-electric-voting)

## Objective

Describe and explore [properties](https://en.wikipedia.org/wiki/Comparison_of_electoral_systems) of a novel (to the best author’s knowledge) [electoral system](https://en.wikipedia.org/wiki/Electoral_system). It’s intuition is based on simple electrical circuits. It orders candidates by assigning each one a potential in such a way that it minimizes the sum of squares of differences of each voter’s intent.


## Background

Basic knowledge of simple electrical circuits is required to describe the intuition behind this system. In particular, static circuits with ideal voltage sources and resistors, satisfying [Kirchhoff's circuit laws](https://en.wikipedia.org/wiki/Kirchhoff%27s_circuit_laws). However it’s not necessary for understanding the derived mathematical properties.


## Electrical intuition

Each voter orders candidates according to their preferences. Then an electrical circuit is created where nodes correspond to candidates, and where for each consecutive pair of candidates from the voter’s list, an [ideal 1V voltage source](https://en.wikipedia.org/wiki/Voltage_source) in series with a 1𝛺 resistor is placed between the corresponding nodes. The voltage source represents the voter’s **intent** that one candidate’s potential is ideally 1V higher than the next one’s potential. The resistor represents the voter’s **ability to influence** the outcome.

This way a chain circuit is formed connecting the candidates from the top-selected one to the last one. In such a circuit the potential of _i-_th candidate is _(i-1)_ V (volts) below the top-selected one.

Then the circuits from all candidates are merged together, sharing the nodes corresponding to candidates. This combined circuit stabilizes in a configuration that is in balance with respect to the voltage sources and resistors. The relative potential of all the candidates then determines their overall ranking.


### Energy

Let’s focus on a single edge of a combined circuit added on behalf of a particular voter between candidates _i_ and _j_, intending _i’_s potential_ _to be 1V higher than _j’_s. Thermal energy dissipated by its resistor is determined by _((V<sub>i</sub>-V<sub>j</sub>)-U)<sup>2</sup>G_ where _V<sub>i</sub>_ and V<sub>j</sub> are the final, balanced potentials of the candidates, _U_ is 1V and G is 1𝛺<sup>-1</sup> [conductance](https://en.wikipedia.org/wiki/Electrical_resistance_and_conductance) of the resistor.

We can see that the energy corresponds to the **square of the deviation** from the voter’s intent. Informally we can say that the more heat the resistor dissipates, the more unhappy the voter is with the outcome for this particular preference.

It can be shown that the circuit stabilizes in a configuration where the total sum of dissipated energy is minimized, and that this property is equivalent to the [Kirchhoff's current law](https://en.wikipedia.org/wiki/Kirchhoff%27s_circuit_laws#Kirchhoff's_current_law). That is, the **candidate’s potentials minimize the sum of squares** of the deviations from all voters’ intents.


## Mathematical definition

[To be done.]


## Properties

[To be done.]


## Contributions and future plans

Please see [Code of Conduct](docs/code-of-conduct.md) and [Contributing](docs/contributing.md).
