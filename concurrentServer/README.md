Concurrent server model  
=======================================  
  
Server runs on 5002 port  
  
Control commands:  
  
PUT -- put data to server. Format key:data (Example: PUT /test/img:ggffhhgg)  
GET -- get data from server. Format key (Example: GET /test/img)  
DELETE -- remove data from server. Format key (Example: DELETE /test/img)
POST -- clever operation which can accept only key (just like DELETE), and accept key:value (just like PUT)  
  
Also, answer will come in json format  
  
Result for GET:  
  
{  
	"action":"READ",  
	"state":"FAIL",  
	"data":"<no data>"  
}  
  
Result for PUT:  
  
{  
	"action":"WRITE",  
	"state":"OK",  
	"key":"ggg"  
}  
  
Result for DELETE:  
  
{  
	"action":"DEL",  
	"state":"OK",  
	"key":"ggg"  
}  
  
If something will go wrong variable "state" will be changed to "FAIL"
