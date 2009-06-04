
module Client.Tests where
import Client.Facebook
import Client.Login
import Test.HUnit



baseConfig = FacebookConfig 
             {apiKey = "80d6cc5ca397c92c9cd41cfe09380b9d",
              secretKey = "64f66142cb325b26cc535f5bf2646957", 
              endPoint = "http://api.facebook.com/restserver.php"}

fb = runFacebook baseConfig 

testFql = TestCase $ do
            result <- fb $ do 
              showLoginScreen
              fql_query "select name from user where uid=4842"
              --friends_get 
            print result
            assertEqual "Test" "hello" "hello"

testStatus = TestCase $ do
               result <- fb $ do 
                           showLoginScreen
                           askPermission "status_update"
                           status_set "hackathon"
               print result
               assertEqual "Test" "hello" "hello"

tests = TestList [testStatus]