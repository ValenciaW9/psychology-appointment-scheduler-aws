import Test.Hspec
import Scheduler (psychologyScheduler)

main :: IO ()
main = hspec $ do
  describe "psychologyScheduler" $ do
    it "should schedule an appointment with a psychologist" $ do
      let result = psychologyScheduler "John Doe" "2022-01-01 10:00" "Dr. Smith"
      result `shouldBe` "Appointment scheduled for John Doe on 2022-01-01 10:00 with Dr Smith"