# loadsharing

Prototype of a routing algorithm to share loads between trusts in the UK. Specifically designed to serve as an algorithm for ICU patients loadshare, but flexible enought to be re-used for other resource sharing endeavour.

Compiling via console:

\texttt{gfortran -c loadsharing_module.f90}

> gfortran loadsharing_poc.f90 loadsharing_module.o

> ./a.out

Description of the code and algorithm:
- The code takes as an input the projected ICU demand at the trust level.
This quantity is estimated for each local authority code based on XXX, and then aggregated at the trust level (each trust corresponds to a set of hospitals).
- The projected ICU demand is compared with the baseline number of available ICU beds for each trust, and from this a local stress level is extracted. 
- If locally a given trust is projected to be 'collapsed' (more demand than capacity), then the algorithm checks whether at least one trust in its neighborhood can accept transfers. If more than one can do it, the algorithm selects the destination at random and assigns a solidary amount (50% of the vacant capacity of the receptor, or the full amount needed, whichever is smaller)
- the last step is performed in parallel for all trusts. Once this is done, the resulting net global stress is computed.
- the algorithm makes 10,000 realisations of this, in each of the realisations different stochastic choices are made for the transfer in each collapsed trust. Each realisation has an attached net global stress reduction, and the algorithm selects the optimal one, and performs the changes.

Comment: as it stands, the algorithm inputs a constant ICU demand per trust (=20), this needs to be changed and put estimated ICU demand.




