# SystProp

Systematic study of the topological properties of real-world complex networks.

## Presentation
This set of R scripts was designed for the analysis of the topological properties of real-world complex networks. The idea is to follow the ideas presented in two seminal previous papers : 

+ [_Collective dynamics of 'small-world' networks_, D. J. Watts and S. H. Strogatz, Nature, 393(6684):440–442, 1998.](http://labs.yahoo.com/files/w_s_NATURE_0.pdf)
+ [_Emergence of scaling in random networks_, Albert-László Barabási and Réka Albert, Science, 286(5439):509–512, 1999.](http://www.nd.edu/~networks/Publication%20Categories/03%20Journal%20Articles/Physics/EmergenceRandom_Science%20286,%20509-512%20(1999).pdf)

In the former, Watts and Strogatz study how the average distance between two nodes behaves as a function of the network size. We propose here to make this study systematic, by considering more data and more topological measures. For this purpose, we look for a functional link between any pair of topological measures on a dataset of about 500 networks. The measures are chosen amongst the most widespread in the complex networks literature.

In the latter article, Barabási and Albert study the distribution of the degree in real-world complex networks, and notice it follows a power-law. We propose to do the same for all the main nodal measures.

In a third step of our work, we try do identify groups of networks depending on those results.

## Use
TODO


## Contact
These scripts were written by [Vincent Labatut](https://galatasaray.academia.edu/VincentLabatut), 2013-2014, <[vlabatut@gsu.edu.tr](mailto:vlabatut@gsu.edu.tr)>, Computer Science Department, Galatasaray University, Istanbul, Turkey. 

They rely heavily on the excellent [igraph library](http://igraph.sourceforge.net/).

If you use them, please cite the paper "".

## License
_This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version._

_This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details._

_You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>._