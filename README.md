## [Colab](https://colab.research.google.com/github/emilemathieu/illegal_fishing/blob/master/yearly.ipynb)

## [Document](https://docs.google.com/document/d/1NTCS4BisS78g7oFP0l3vq37_5zN2wf-9Gz0ixvrsAb4/edit?usp=sharing)

## Python setup
Install Conda: https://docs.conda.io/projects/conda/en/latest/user-guide/install/
```
conda create -n example_environment python=3.9
conda activate example_environment
conda install -c conda-forge pymc3 jupyter nbstripout arviz
nbstripout --install
git config filter.nbstripout.extrakeys 'metadata.celltoolbar metadata.kernelspec.display_name metadata.kernelspec.language metadata.kernelspec.name metadata.kernelspec metadata.language_info metadata.language_info.pygments_lexer metadata.language_info.version metadata.toc metadata.notify_time metadata.varInspector cell.metadata.heading_collapsed cell.metadata.hidden cell.metadata.code_folding cell.metadata.tags cell.metadata.init_cell cell.metadata.init_cell'
```
