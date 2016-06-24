# conj
* Online Judge for Programs written in C
* ESC101 Project 

# Successfully Deployed
# TimeLine
* Backend for Time and Correctness : February 1, 2016 
* Backend for Memory : February 22, 2016 
* Backend + Frontend for User sign in and submission of Code via web :  March 5, 2016
* Backend + Frontend for Jury : March 5, 2016 
* Optimization of Judgement using Multiple threads : March 2, 2016
* Basic checks for Safe code : March 15, 2016
* Deployment : March 15, 2016
 
# Platforms Used
* Frontend - User management part : Flask interconnected to Haskell using Bash
* Backend for Correctness, Time, Memory, Optimization: Haskell

# Work update
* The folder Judge contains the entire haskell backend for the Judgement
* The learning folder contains Haskell files used while going through the tutorials.
* THe Assignment 1 folder contains solutions to the assignment sent. 
* app.py and other python files are for the file server.
* The bash code is for interlinking the Python front end with Judgement Haskell backend.
* Log files are all text, but actual management is done via the mysql database.
* the setup file contains the mysql procedures required to be present in the mysql environment for Flask to do it's job
* templates contains html files embedded with jinja for embedding elements that take input from the web server and depend on the state of the system and the request.
* The code has been tested for multiple users but just one question due to lack of innovation on the front of creating questions and test cases.
* Scalability is still an issue, while reliability is not.
