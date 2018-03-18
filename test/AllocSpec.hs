module AllocSpec where

import Test.Hspec
import Text.Parsec.Pos (newPos)
import qualified Symbol as Sym
import qualified Environment as Env
import qualified Temporary as Tmp
import qualified Frame
import qualified AST
import FrameExp ( Level(..)
                , Access(..)
                , VarEnv(..)
                , LEnv(..)
                )
import qualified FrameExp as FE
import Alloc

spec :: Spec
spec =
    describe "Alloc" $ do
      let t = Sym.newTable 0
      let (outerLabel, t') = Tmp.newLabel t
      let newFrame = Frame.NewFrame Frame.newX86Frame
      let (tbl, outerFrame) = Frame.newX86Frame outerLabel [] t'
      describe "Access and Level" $ do
        it "returns a new Access that contains the frame and the level and an extra formal for the link" $ do
          let (label, tbl') = Tmp.newLabel tbl
          let (tbl'', level) = newLevel newFrame (Outermost outerFrame) label [Frame.Escape] tbl'

          level `shouldBe`
            Nested (Outermost outerFrame)
                             (Frame.X86Frame { Frame.x86_name = label
                                             , Frame.x86_formals = [ Frame.InFrame 8
                                                                   , Frame.InFrame 12
                                                                   ]
                                             , Frame.x86_locals = []
                                             , Frame.x86_nextOffset = 16
                                             })

        it "does not return the first formal, and includes the level" $ do
          let tbl = Sym.newTable 0
          let (label, tbl') = Tmp.newLabel tbl
          let (tbl'', level) = newLevel newFrame (Outermost outerFrame) label [Frame.Escape] tbl'

          formals level `shouldBe` [Access level (Frame.InFrame 12)]
        it "allocates locals and returns the level" $ do
          let tbl = Sym.newTable 0
          let (label, tbl') = Tmp.newLabel tbl
          let (tbl'', level) = newLevel newFrame (Outermost outerFrame) label [Frame.Escape] tbl'
          let (Nested (Outermost outerFrame) frame) = level

          let ((tbl''', level'), local) = allocLocal Frame.Escape level tbl''
          let ((tbl'''', frame'), local') = Frame.allocLocal Frame.Escape frame tbl''

          level' `shouldBe` Nested (Outermost outerFrame) frame'
          local `shouldBe` Access (Nested (Outermost outerFrame) frame') local'
        it "returns all allocated locals" $ do
          let tbl = Sym.newTable 0
          let (label, tbl') = Tmp.newLabel tbl
          let (tbl'', level) = newLevel newFrame (Outermost outerFrame) label [Frame.Escape] tbl'
          let ((tbl''', level'), local) = allocLocal Frame.Escape level tbl''

          locals level' `shouldBe` [Access level' (Frame.InFrame 16)]
      describe "alloc" $ do
        let dummyPos = newPos "" 1 1
        let iniVEnv = Env.fromList []
        let iniTbl = Sym.newTable 0
        let (outerLabel, iniTbl') = Tmp.newLabel iniTbl
        let (iniTbl'', outerFrame) = Frame.newX86Frame outerLabel [] iniTbl'
        let iniLevel = (Outermost outerFrame)
        let iniVarEnv = (VarEnv { vEnv = iniVEnv
                                , sym = iniTbl''
                                })
        let iniLEnv = LEnv newFrame
                           iniVarEnv
                           iniLevel

        it "allocs a new level for a function" $ do
          let fn = AST.FnDec "add"
                             [("x", "int"), ("y", "int")]
                             (Just "int")
                             (AST.BinOp dummyPos
                                        AST.Addition
                                        (AST.LValExp dummyPos (AST.Id "x"))
                                        (AST.LValExp dummyPos (AST.Id "y")))
          let (label, tbl) = Tmp.newLabel (sym iniVarEnv)
          let (add, tbl') = Sym.put "add" tbl
          let (x, tbl'') = Sym.put "x" tbl'
          let (y, tbl''') = Sym.put "y" tbl''
          let (tbl'''', level) = newLevel newFrame
                                          iniLevel
                                          label
                                          [Frame.Escape, Frame.Escape]
                                          tbl'''
          let newVarEnv = Env.pushScope (zip [x, y] (formals level)) (vEnv iniVarEnv)
          let newLEnv = LEnv newFrame
                             (iniVarEnv { vEnv = newVarEnv
                                        , sym = tbl''''
                                        })
                             level

          let updatedFrame = Frame.X86Frame { Frame.x86_name = outerLabel
                                            , Frame.x86_formals = []
                                            , Frame.x86_locals = [Frame.InFrame 8]
                                            , Frame.x86_nextOffset = 12
                                            }
          let updatedLevel = FE.Outermost updatedFrame
          let updatedVarEnv =  Env.addBinding (add, (head $ locals updatedLevel)) (vEnv iniVarEnv)
          let updatedLEnv = LEnv newFrame
                                 (iniVarEnv { vEnv = updatedVarEnv
                                            , sym = tbl''''
                                            })
                                 updatedLevel

          allocDec iniLEnv fn `shouldBe`
            ( updatedLEnv
            , FE.FnDec newLEnv
                       "add"
                       ["x", "y"]
                       (FE.BinOp dummyPos
                                 newLEnv
                                 AST.Addition
                                 (FE.LValExp dummyPos newLEnv (FE.Id "x"))
                                 (FE.LValExp dummyPos newLEnv (FE.Id "y")))
            )
        it "allocs a new local variable within a let" $ do
          let lEnv = LEnv newFrame iniVarEnv iniLevel
          let exp = AST.Let dummyPos
                            [AST.VarDec "x" Nothing (AST.IntLiteral dummyPos 5)]
                            [AST.LValExp dummyPos (AST.Id "x")]
          let (FE.Let _ (LEnv _ varEnv lvl) _ _) = alloc lEnv exp
          let (Just x) = Sym.get "x" (sym varEnv)
          let (Just access) = Env.lookup x (vEnv varEnv)

          let (x, updatedTable) = Sym.put "x" (sym iniVarEnv)
          let updatedFrame = Frame.X86Frame { Frame.x86_name = outerLabel
                                            , Frame.x86_formals = []
                                            , Frame.x86_locals = [Frame.InFrame 8]
                                            , Frame.x86_nextOffset = 12
                                            }
          let updatedLevel = FE.Outermost updatedFrame
          access `shouldBe` FE.Access updatedLevel (Frame.InFrame 8)
          lvl `shouldBe` updatedLevel
        it "allocs a new local variable for a function within a let" $ do
          let lEnv = LEnv newFrame iniVarEnv iniLevel
          let exp = AST.Let dummyPos
                            [AST.FnDec "add"
                                       [("x", "int"), ("y", "int")]
                                       (Just "int")
                                       (AST.BinOp dummyPos
                                                   AST.Addition
                                                   (AST.LValExp dummyPos (AST.Id "x"))
                                                   (AST.LValExp dummyPos (AST.Id "y")))]
                            [AST.FunctionCall dummyPos
                                              "add"
                                              [ (AST.IntLiteral dummyPos 1)
                                              , (AST.IntLiteral dummyPos 2)]]
          let (FE.Let _ (LEnv _ varEnv lvl) decs fexps) = alloc lEnv exp
          let (Just add) = Sym.get "add" (sym varEnv)
          let (Just access) = Env.lookup add (vEnv varEnv)

          let (add, updatedTable) = Sym.put "add" (sym iniVarEnv)
          let updatedFrame = Frame.X86Frame { Frame.x86_name = outerLabel
                                            , Frame.x86_formals = []
                                            , Frame.x86_locals = [Frame.InFrame 8]
                                            , Frame.x86_nextOffset = 12
                                            }
          let updatedLevel = FE.Outermost updatedFrame
          access `shouldBe` FE.Access updatedLevel (Frame.InFrame 8)
          lvl `shouldBe` updatedLevel
        it "allocs a new local variable for the iterator in a for loop" $ do
          let lEnv = LEnv newFrame iniVarEnv iniLevel
          let exp = AST.For dummyPos
                            "i"
                            (AST.IntLiteral dummyPos 0)
                            (AST.IntLiteral dummyPos 5)
                            (AST.IntLiteral dummyPos 10)

          let (FE.For _(LEnv _ varEnv lvl) "i" _ _ _) = alloc lEnv exp
          let (Just i) = Sym.get "i" (sym varEnv)
          let (Just access) = Env.lookup i (vEnv varEnv)

          let (i, updatedTable) = Sym.put "i" (sym iniVarEnv)
          let updatedFrame = Frame.X86Frame { Frame.x86_name = outerLabel
                                            , Frame.x86_formals = []
                                            , Frame.x86_locals = [Frame.InFrame 8]
                                            , Frame.x86_nextOffset = 12
                                            }
          let updatedLevel = FE.Outermost updatedFrame
          access `shouldBe` FE.Access updatedLevel (Frame.InFrame 8)
          lvl `shouldBe` updatedLevel
