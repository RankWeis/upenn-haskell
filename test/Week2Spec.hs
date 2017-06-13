module Week2Spec (spec) where

import Week2
import Log
import Test.Hspec
import Control.Exception (evaluate)

spec :: Spec

sampleMessages = [
    LogMessage (Error 50) 148 "Error 50",
    LogMessage (Error 55) 148 "Error 55",
    LogMessage Info 148 "Info",
    LogMessage (Error 20) 148 "Error 20"]

spec = do
  describe "ParseMessageType" $ do
    it "returns the error code" $ do
        parseMessageType "E 1 aoeu" `shouldBe` (Error 1, "aoeu")
    it "returns the multiple digit error code" $ do
        parseMessageType "E 10 aoeu" `shouldBe` (Error 10, "aoeu")
    it "returns a warning" $ do
        parseMessageType "W 243 aoeu" `shouldBe` (Warning, "243 aoeu")
    it "returns an info" $ do
        parseMessageType "I 243 aoeu" `shouldBe` (Info, "243 aoeu")
  describe "parseMessage" $ do
    it "returns an error message" $ do
        parseMessage "E 2 148 #56k istereadeat lo d200ff] BOOTMEM" `shouldBe` (LogMessage (Error 2) 148 "#56k istereadeat lo d200ff] BOOTMEM")
    it "returns an info message" $ do
        parseMessage "I 147 mice in the air, I’m afraid, but you might catch a bat, and" `shouldBe` (LogMessage Info 147 "mice in the air, I’m afraid, but you might catch a bat, and")
  describe "whatWentWrong" $ do
    it "only returns errors over 50" $ do
        length (whatWentWrong sampleMessages) `shouldBe` 2
