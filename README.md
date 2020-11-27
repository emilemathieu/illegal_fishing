## [Colab](https://colab.research.google.com/drive/1e2gjYbYl9vI1JosuVsvGrucdqNJqM_mf#scrollTo=la0a3fUFKRUE)

## Python setup
Install Conda: https://docs.conda.io/projects/conda/en/latest/user-guide/install/
```
conda create -n example_environment python=3.9
conda activate example_environment
conda install -c conda-forge pymc3 jupyter nbstripout arviz
nbstripout --install
git config filter.nbstripout.extrakeys 'metadata.celltoolbar metadata.kernelspec.display_name metadata.kernelspec.language metadata.kernelspec.name metadata.kernelspec metadata.language_info metadata.language_info.pygments_lexer metadata.language_info.version metadata.toc metadata.notify_time metadata.varInspector cell.metadata.heading_collapsed cell.metadata.hidden cell.metadata.code_folding cell.metadata.tags cell.metadata.init_cell cell.metadata.init_cell'
```
