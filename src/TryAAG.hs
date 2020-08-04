{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UnicodeSyntax             #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TryAAG where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Data.Singletons
import Data.GenRec

$(addNont "Root")
$(addNont "Tree")



$(addProd "Leaf" ''Nt_Tree [("val", Ter ''Int)])
$(addProd "Node" ''Nt_Tree [("l", NonTer ''Nt_Tree),("r", NonTer ''Nt_Tree)])
$(addProd "Root" ''Nt_Root [("tree",NonTer ''Nt_Tree)])



data Root = Root Tree deriving Show
data Tree = Leaf Int | Node Tree Tree deriving Show

t1 = Node (Leaf 4) (Node (Leaf 5) (Leaf 1))

sem_Root asp (Root t)
 = knitAspect p_Root asp $ ch_tree .=. sem_Tree asp t .*. EmptyRec
sem_Tree asp (Leaf i)
 = knitAspect p_Leaf asp $ ch_val .=. sem_Lit i .*. EmptyRec
sem_Tree asp (Node l r)
 = knitAspect p_Node asp
 $   ch_l .=. sem_Tree asp l
 .*. ch_r .=. sem_Tree asp r
 .*. EmptyRec

$(attLabels [("sres", ''Tree), ("smin", ''Int), ("ival", ''Int)])

asp_smin
  =
 syn smin p_Leaf (ter ch_val) .+:
 syn smin p_Node (min @ Int <$> at ch_l smin <*> at ch_r smin) .+:
 emptyAspect

asp_sres
 =    syn sres p_Leaf (Leaf <$> at lhs ival)
 .+:  syn sres p_Node (Node <$> at ch_l sres <*> at ch_r sres)
 .+:  syn sres p_Root (at ch_tree sres)
 .+:  emptyAspect

asp_ival
 =    inh ival p_Root ch_tree (at ch_tree smin)
 .+:  inh ival p_Node ch_l (at lhs ival)
 .+:  inh ival p_Node ch_r (at lhs ival)
 .+:  emptyAspect

asp_repmin
  = asp_smin .:+: asp_sres .:+: asp_ival

mini t = sem_Tree asp_smin t emptyAtt #. smin

repmin t
  = sem_Root asp_repmin (Root t)
         emptyAtt #. sres

tryAAG = repmin $ (Node (Leaf 5) (Node (Leaf 2) (Leaf 1)))
