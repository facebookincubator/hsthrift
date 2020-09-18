module Facebook.Init where

withFacebook :: IO () -> IO ()
withFacebook = id

withFacebookUnitTest :: IO a -> IO a
withFacebookUnitTest = id
