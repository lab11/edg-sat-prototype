
module Control.Lens.TupleLens where

import Control.Lens

-- | Given two disjoint lenses into the same structure, this function creates
--   a lens that return a tuple with those elements of the structure.
--
--   Do not use this with lenses that are not dijoint, since that would violate
--   the lens laws and generally break shit.
makeTup2Lens :: Lens' s a -> Lens' s b -> Lens' s (a,b)
makeTup2Lens al bl = lens getter setter
  where
  getter s = (s ^. al, s ^. bl)
  setter s (a,b) =  (bl .~ b) . (al .~ a) $ s

-- | Given three disjoint lenses into the same structure, this function creates
--   a lens that return a tuple with those elements of the structure.
--
--   Do not use this with lenses that are not dijoint, since that would violate
--   the lens laws and generally break shit.
makeTup3Lens :: Lens' s a -> Lens' s b -> Lens' s c -> Lens' s (a,b,c)
makeTup3Lens al bl cl = lens getter setter
  where
  getter s = (s ^. al, s ^. bl, s ^. cl)
  setter s (a,b,c) = (cl .~ c) . (bl .~ b) . (al .~ a) $ s

-- | Given four disjoint lenses into the same structure, this function creates
--   a lens that return a tuple with those elements of the structure.
--
--   Do not use this with lenses that are not dijoint, since that would violate
--   the lens laws and generally break shit.
makeTup4Lens :: Lens' s a -> Lens' s b -> Lens' s c -> Lens' s d ->  Lens' s (a,b,c,d)
makeTup4Lens al bl cl dl = lens getter setter
  where
  getter s = (s ^. al, s ^. bl, s ^. cl, s ^. dl)
  setter s (a,b,c,d) = (dl .~ d) . (cl .~ c) . (bl .~ b) . (al .~ a) $ s

-- | Given five disjoint lenses into the same structure, this function creates
--   a lens that return a tuple with those elements of the structure.
--
--   Do not use this with lenses that are not dijoint, since that would violate
--   the lens laws and generally break shit.
makeTup5Lens :: Lens' s a -> Lens' s b -> Lens' s c -> Lens' s d -> Lens' s e -> Lens' s (a,b,c,d,e)
makeTup5Lens al bl cl dl el = lens getter setter
  where
  getter s = (s ^. al, s ^. bl, s ^. cl, s ^. dl, s ^. el)
  setter s (a,b,c,d,e) = (el .~ e) . (dl .~ d) . (cl .~ c) . (bl .~ b) . (al .~ a) $ s

