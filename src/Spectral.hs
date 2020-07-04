{-# LANGUAGE FlexibleContexts #-}

module Spectral(layout,default_layout) where

import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Data

degree_matrix m =
  let a = foldl1 (+) (toRows m)
      b = takeDiag m
      in
    a - b

compute_laplacian adj_matrix node_weights =
  let (n,m) = size adj_matrix
      weighted_adjency = adj_matrix
      mat_D = diag $ degree_matrix weighted_adjency
      mat_L = mat_D - weighted_adjency
      in
      (mat_L,mat_D)

symmetrize m = m + tr m

layout adjacent_matrix node_weights =
  let sym_adjacent_matrix = symmetrize adjacent_matrix
      (mat_L,mat_D) = compute_laplacian sym_adjacent_matrix node_weights
      eigenVectors = toColumns $ snd $ geigSH' mat_L mat_D
      v2 = eigenVectors !! 1
      v3 = eigenVectors !! 2
  in
    [(v2  ! i, v3 ! i) | i <- [0..size v2-1] ]

default_layout adjacent_matrix =
  let (n,_) = size adjacent_matrix
      ones = matrix n [1 | i <- [1..n]]
  in
    layout adjacent_matrix ones
