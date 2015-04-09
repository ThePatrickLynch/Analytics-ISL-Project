rm(list=ls())

library(RMySQL)

#connect to database
mydb = dbConnect(MySQL(), user='pdspl', password='GiveMeTheStats', host='localhost', dbname="student_retention")

# execute select command and bring data into data.frame, takes quite a while - 
# is it quicker to bring files separately and merge

myevents = fetch(dbSendQuery(mydb, "SELECT * FROM sakai_event INNER JOIN sakai_session  ON 
sakai_event.SESSION_ID=sakai_session.SESSION_ID 
INNER JOIN sakai_user_id_map ON sakai_session.Session_USER=sakai_user_id_map.USER_ID
WHERE sakai_event.CONTEXT='cmpst_00699';"))

