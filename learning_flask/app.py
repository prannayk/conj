#external imports
from flask import Flask, render_template,request, json, redirect, session
from flask.ext.bcrypt import Bcrypt
from flask.ext.mysql import MySQL
from werkzeug import generate_password_hash, check_password_hash

#flask settings
app = Flask("learning_flask")
bcrypt = Bcrypt(app)
app.secret_key = "tobeornottobe"

#mysql settings
mysql = MySQL()
app.config['MYSQL_DATABASE_USER'] = 'root'
app.config['MYSQL_DATABASE_PASSWORD'] = 'drasnac32'
app.config['MYSQL_DATABASE_DB'] = 'conj'
app.config['MYSQL_DATABASE_HOST'] = 'localhost'
mysql.init_app(app)

conn = mysql.connect()
cursor = conn.cursor()

#routes
@app.route("/")
def main() : 
	return render_template("index.html")
@app.route("/showSignUp")
def showSignUp() :
	return render_template("signup.html")
@app.route('/signUp',methods=['POST'])
def signUp() : 
	_name = request.form['inputName']
	_email = request.form['inputEmail']
	_password = request.form['inputPassword']
	_hashed_password = bcrypt.generate_password_hash(_password)
	cursor.callproc('lp_createUser',(_name,_email,_hashed_password))
	conn.commit()
	return redirect('/')

@app.route("/showSignIn")
def showSignIn() : 
	return render_template("signin.html")

@app.route("/userHome")
def userHome() :
	if(session.get('user')):
		return render_template("userHome.html")
	else:
		return render_template("error.html",error="Unauthorized access to restricted pages")

@app.route("/logout")
def logout() :
	session.pop('user',None)
	return redirect('/')

@app.route("/signIn",methods=['POST'])
def signIn() :
	try : 
		_username = request.form['inputEmail']
		_password = request.form['inputPassword']
		cursor.callproc('dp_validateLogin',(_username,))
		data = cursor.fetchall()

		if len(data) > 0:
			if bcrypt.check_password_hash(str(data[0][3]),_password):
				session['user'] = data[0][0]
				return redirect('/userHome')
			else :
				return render_template('error.html',error='Username or Password incorrect')
		else:
			return render_template('error.html',error='Username not registered')
	except Exception as e:
		return render_template('error.html',error = e)

#app-runnings
app.debug = True
app.run()