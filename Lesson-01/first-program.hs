messyMain :: IO ()
messyMain = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print ("Dear " ++ recipient ++ ", \n" ++ "Thanks for buying " ++ title ++ "\nThanks,\n" ++ author)

createToPart recipient = "Dear " ++ recipient ++ ",\n"

createBodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ",\n"

createFromPart author = "Thanks, \n" ++ author

createEmail recipient bookTitle author =
  createFromPart recipient
    ++ createBodyPart bookTitle
    ++ createFromPart author

main :: IO ()
main = do
  print "Main"
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  bookTitle <- getLine
  print "Who is the Author"
  author <- getLine
  print (createEmail recipient bookTitle author)
