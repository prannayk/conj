from __future__ import print_function
from flask import Flask, request, json, render_template, session, redirect
from flask.ext.bcrypt import Bcrypt
from flask.ext.mysql import MySQL
from subprocess import call
import sys

app = Flask("Judge")
bcrypt = Bcrypt(app)
app.secret_key = "tobeornot3be"

mysql = MySQL()
app.config['MYSQL_DATABASE_USER'] = 'root'
app.config['MYSQL_DATABASE_PASSWORD'] = 'drasnac32'
app.config['MYSQL_DATABASE_DB'] = 'conj'
app.config['MYSQL_DATABASE_HOST'] = 'localhost'
mysql.init_app(app)

class user():
	def __init__(self,name,score,rank):
		self.name = name
		self.score = score
		self.rank = rank
	def __init__(self,name,score,rank):
		self.name = name
		self.score = score
		self.rank = rank

class navigation():
	def __init__(self,title,url):
		self.title = title
		self.url = url
	def change(self,title,url,name):
		self.name = name
		self.title = title
		self.url = url
	def __call__(self,title,url):
		self.title = title
		self.url = url
		self.name = None
fields = navigation('Sign In','showSignIn')
class placeholders():
	def __init__(self,name,username,password,email):
		self.name = name
		self.username = username
		self.password = password
		self.email = email
	def __call__(self,name,username,password,email):
		self.name = name
		self.username = username
		self.password = password
		self.email = email

class ques_list():
	def __init__(self,head):
		self.head = head
		self.questions = []

class qdesc():
	def __init__(self,title,vattempt,attempt,content,id):
		self.title = title
		self.vattempt = vattempt
		self.attempt = attempt
		self.content = content
		self.link = 'question/'+str(id)

def question(id):
	conn = mysql.connect()
	cursor = conn.cursor()
	cursor.callproc('getQues',(id,))
	data = cursor.fetchall()
	if len(data) is 0 :
		return None
	else :
		return qdesc(data[0][1],str(data[0][8]),str(data[0][9]),str(data[0][5]),str(id))
def checkAccess():
	if session.get('user'):
		conn = mysql.connect()
		cursor = conn.cursor()
		cursor.callproc('getUser',(str(session['user']),))
		data = cursor.fetchall()
		if len(data) is 0 :
			session['juryaccess'] = 0
		else :
			session['juryaccess'] = int(data[0][7])
	else :
		session['juryaccess'] = 0


def writeCase(inputt,outputt,name,title):
	with open('Judge/'+name+'/'+title,"w") as inputf:	
		inputf.write(inputt)	
	outputf = open('Judge/ans/'+name+'/'+title,"w")
	outputf.write(outputt)
	outputf.close()

@app.route("/question/<quesid>",methods=['GET'])
def render_question(quesid):
	if quesid: 
		return render_template('question.html',fields=fields,ques=question(quesid))
	else:
		return redirect('/userhome')

@app.route("/")
def main():
	if session.get('user'):
		fields.change('Logout','logout',session['user'])
	return render_template("index.html",fields=fields,active='home')

@app.route("/jury")
def jury():
	try :
			if int(session['juryaccess']) == 1:
				conn = mysql.connect()
				cursor = conn.cursor()
				cursor.callproc('getQuestions',((str(session['user'])),))
				data = cursor.fetchall()
				access = ques_list('header')
				for i in range(len(data)):
					access.questions.append(ques_d(data[i]))
				if len(access.questions) is 0 : 
					access.head = None
				cursor.close()
				conn.close()
				return render_template('jury.html',fields=fields,questions=access)
			else :
				return render_template('error.html',fields=fields,error='Access to this part is restricted. If you are seeing this, please go away and never come back!')
	except Exception as e :
		return render_template('error.html',fields=fields,error=e) 

@app.route("/userHome")
def userHome():
	try : 
		if session.get('user') : 
			conn = mysql.connect()
			cursor = conn.cursor()
			cursor.callproc('getUser',((str(session['user'])),))
			data = cursor.fetchall()
			attempted = ques_list('header')
			unattempted = ques_list('header')
			for i in range(7,len(data[0])):
				if data[0][i] == 0:
					unattempted.questions.append(question(i-6))
				else :
					attempted.questions.append(question(i-6))
			if len(attempted.questions) is 0:
				attempted.head = None
			if len(unattempted.questions) is 0:
				unattempted.head = None
			cursor.close()
			conn.close()
			return render_template('userhome.html',fields=fields,unattempted=unattempted,attempted=attempted)
		else : 
			return render_template('signin.html',fields=fields)
	except Exception as e:
		return render_template('error.html',error=e,fields=fields)

@app.route("/jury/addq")
def addq():
	return render_template("addq.html",fields=fields)

@app.route("/jury/addQuestion",methods=['POST'])
def addQuestion():
	try :
		test_case = []
		_num = int(request.form['case_count'])
		title = request.form['question-title']
#		call(["cd ~/ESC101/conj/Judge"])
#		call(["mkdir ~/ESC101/conj/Judge/"+title])
#		call(["mkdir Judge/ans/"+title,""])
		for i in range(_num):
			writeCase(request.form['test'+i+'-input'],request.form['test'+i+'-output'],request.form['test'+i+'name'],title)
		return 'Success'
	except Exception as e : 
		return str(e)

@app.route("/showSignIn")
def showSignIn():
	if(session.get('user')) :
		return redirect("/")
	else:
		return render_template("signin.html",fields=fields)

@app.route("/signIn",methods=['POST'])
def signIn():
	try : 
		conn = mysql.connect()
		cursor = conn.cursor()
		_username = request.form['inputUsername']
		_password = request.form['inputPassword']
		conn.commit
		cursor.callproc('getUser',(_username,))
		data = cursor.fetchall()
		if len(data) > 0:
			if(bcrypt.check_password_hash(str(data[0][3]),_password)):
				session['user'] = str(data[0][1])
				session['juryaccess'] = str(data[0][7])
				fields.change('Logout','logout',session['user'])
				return redirect('/')
			else:
				return render_template('error.html',error='Username and Password do not match!',fields=fields)
		else:
			return render_template('error.html',error='Username does not exist',fields=fields)
	except Exception as e : 
		return render_template('error.html',error=e,fields=fields)

@app.route("/showSignUp",)
def showSignUp():
	if not session.get('user'):
		return render_template("signup.html",error='',placeholder=placeholders,fields=fields)
	else:
		return redirect('/')

@app.route('/logout')
def logout() :
	session.pop('user',None)
	fields('Sign In','showSignIn')
	return redirect('/')

@app.route('/leaderBoard')
def leaderBoard():
	try :
		conn = mysql.connect()
		cursor = conn.cursor()
		cursor.callproc('listLeaders',())
		data = cursor.fetchall()
		users = []
		if len(data) > 0 :
			for i in range(len(data)):
				this = user(str(data[i][1]),str(data[i][6]),i+1)
				users.append(this)
			return render_template('leaders.html',fields=fields,users=users)
		else:
			return render_template('leaders.html',fields=fields,header='The game is not up yet. Hold your horses!')
	except Exception as e:
		return render_template('error.html',error=e,fields=fields)

@app.route("/signUp",methods=['POST'])
def signUp():
	try :
		conn = mysql.connect()
		cursor = conn.cursor()
		_name = request.form['inputName']
		_password = bcrypt.generate_password_hash(request.form['inputPassword'])
		_email = request.form['inputEmail']
		_username = request.form['inputUsername']
		_selective_hash = bcrypt.generate_password_hash(_username)
		cursor.callproc('createUser',(_name,_email,_username,_password,_selective_hash))
		data = cursor.fetchall()
		if len(data) is 0:
			conn.commit()
			return render_template('confirmmail.html',fields=fields)
			session['confirm'] = False
		else:
			return redirect("/")
	except Exception as e:
		return render_template('error.html',error=e,fields=fields)

app.debug = True
app.run()
