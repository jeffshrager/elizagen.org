#! /usr/bin/python3

#modules i'm using
import praw #python wrapper for the reddit api, lets you connect to the site
import obot #keeps authorization keys in a separate file for privacy
import sqlite3 #database
import random #does math stuff
 
#variables to use below that i might want to change
USER = 'ladymouth'
DATABASE = 'ladymouth.db' #do i need to make this a fixed path so that the files match up?
SUBREDDIT = 'Masculism' #'MensRights+TheRedPill+Masculism' +etc
# NEW PROBLEM: Many terms will be subreddit specific, how to target the database repeatedly VS more random?
WAIT = 600
LIMIT = 200
KEYWORD = ''
 
# authentication
r = obot.login()
 
class SQLmanager():
    def __init__(self, db):
        self.conn = sqlite3.connect(db)
        self.cur = self.conn.cursor()
 
    def query(self, *args):
        self.cur.execute(*args)
        self.conn.commit()
        return self.cur
   
    def close(self): #where to close / __exit__ __del__ (with statement?)
        self.conn.close()
 
 
class Quote():
    def __init__(self, quoteID, author, body, tag):
        self.qid = quoteID
        self.author = author
        self.body = body
        self.tag = tag
 
    @staticmethod
    def pick_random():
        global q
        query = "SELECT * FROM quotations ORDER BY RANDOM() LIMIT 1"
        for row in sql.query(query):
            quoteID, author, body, tag = row #assigns sql output to the variables to pass to the class
        q = Quote(quoteID, author, body, tag) #sends variables to create object from class
        #should all these be separated into their own method?
        return q
 
    @staticmethod
    def pick_tag(tag):
        global q
        query = "SELECT * from quotations WHERE tag=?"
        for row in sql.query(query, (tag,)):
            quoteID, author, body, tag = row  #replace with ??? Quote.map_variables()
        q = Quote(quoteID, author, body, tag) #sends variables to create object from class
        return q
 
    @staticmethod
    def pick_ID(quoteID):
        global q
        query = "SELECT * FROM quotations WHERE quoteID=?"
        for row in sql.query(query, (quoteID,)):
            quoteID, author, body, tag = row
        q = Quote(quoteID, author, body, tag)
        return q
 
    #create a method that finds the last posted reply, pulls the next quoteID from it.
    @staticmethod
    def findlastused():
        query = "SELECT quoteID FROM replied ORDER BY quoteID DESC LIMIT 1;"
        for row in sql.query(query):
            lastID = row #NEED TO REMOVE FROM TUPLE HERE
        quoteID = int(lastID) + 1
        Quote.pickID(quoteID)
 
    def display(self):
        print(self.body + ' (' + self.tag + ')')
 
''' @staticmethod
   def map_variables():
       global q
       row = pick_tag
       #should all these be separated into their own method?
       quoteID, author, body, tag = row #assigns sql output to the variables to pass to the class
       q = Quote(quoteID, author, body, tag) #sends variables to create object from class
       return q'''
 
 
class Post():
    def __init__(self, pid, author, body, q):
        self.pid = pid
        self.author = author
        self.body = body
        self.q = q #binds the quote object to the post
        self.reply = self.q.body + ' -' + self.q.author
 
    def display(self):
        print(self.author + ' says: ' + self.body)
        print('Would reply: ' + self.reply)
 
    @staticmethod
    def get():
        #make posts global to save the string and not have to redo the search?
        posts = r.get_subreddit(SUBREDDIT).get_comments(limit = LIMIT)
        posts = praw.helpers.flatten_tree(posts)
        return posts
           
    # HOW TO RETURN MORE THAN ONE MATCH??????
    @staticmethod
    def match_random(q):
        global match
        posts = Post.get()
        tag = q.tag
        matches = [post for post in posts if tag.lower() in post.body.lower()]
        if matches: #to make multiple: for match in matches, try reply_with
            match = random.choice(matches)
            Post.create(match)
            return match
        else:
            print('No matches found. Try again.')
            exit() #kill program here or try again?
            # would this be in a while loop? ADD A HANDLER
            #Quote.pick_random()
            #Post.match_random(q)
 
    #trying a new version that breaks up the steps into separate functions, tries returning multiple matches
    @staticmethod
    def match(q):
        posts = Post.get()
        tag = q.tag
        matches = [post for post in posts if tag.lower() in post.body.lower()]
        return matches
 
    @staticmethod
    def try_all():
        matches = Post.match(q)
        matchlist = list()
        if matches:
            for match in matches: #try reply with and log
                item = Post.create(match) #won't this just write over each version?, make a list of variables here to assign to each, or dispense with objects altogether
                matchlist.append(item) #how to assign different names to each
            return False
        else: #how to rerun, put in while loop instead?
            '''Quote.pick_random()
           q.display()
           Post.try_all()'''
            return True
 
    @staticmethod
    def create(m):
        global p
        pid = str(m.id)
        author = str(m.author)
        body = str(m.body)
        p = Post(pid, author, body, q)
        return p
 
    def check(self):
        sql.query("CREATE TABLE IF NOT EXISTS replied(pid TEXT, author TEXT, body TEXT, quoteID INTEGER)")
        query = "SELECT * FROM replied WHERE pid=?"
        sql.query(query, (self.pid,))
        already = sql.cur.fetchone()
        if already:
            print('Already replied to this post.')
            return True
        else:
            #print('Could reply.')
            return False
     
    def reply_with(self):
        if (self.check() is False) and (self.author != USER):  #checks that author is not bot--does this work?
            try:
                #UNCOMMENT TO GO LIVE
                #match.reply(self.reply)
                print('REPLIED: ' + self.reply)
                print('TO THIS POST: ' + str(match.body)) #check
                #log post -- UNCOMMENT TO GO LIVE
                #try:
                    #self.log()
                #except:
                    #print('Replied but could not log.')
            except:
                print('Could not reply.')
        else:
            print('Already replied or self-reply')
 
    def log(self):
        try:
            ins = "INSERT INTO replied (pid, author, body, quoteID) VALUES(?, ?, ?, ?)"
            sql.query(ins, (self.pid, self.author, self.body, self.q.qid))
            print('Logged post in "replied"')
        except:
            print('Something went wrong. Post not logged.')
 
         
'''class Submission(Post):
   @staticmethod
   def get():
       posts = r.get_subreddit(SUBREDDIT).get_new(limit = LIMIT)
       return posts
   @staticmethod
   def search(tag): #tag, how to get this worked in? what's being passed into it?
       # how to call in the inherited class rather than override it. super().__init__(self)
       posts = r.search(tag, subreddit=SUBREDDIT, sort=u'new', limit=LIMIT)
       return posts
   def read():
       print([post.selftext for post in posts])
   def __eq__(self, tag):
       return self.body.lower() == tag.lower()
'''
 
class Response(Post):
    def __init__(self, pid, author, body, botid):
        self.pid = pid
        self.author = author
        self.body = body
        self.botid = botid #binds the response to the bot's post
 
    @staticmethod
    def get_bot_posts():
        global responses
        posts = r.get_redditor(USER).get_comments(sort=u'new')
        refreshed = [post.refresh() for post in posts]
        return refreshed
 
    @staticmethod
    def get_replies():
        resps = []
        refreshed = Response.get_bot_posts()
        for post in refreshed:
            #print(str(post.replies)) #THIS WORKED because it was trying to read blanks before
            resps += post.replies
        return resps
       
    @staticmethod
    def create(resp):
        global b
        pid = str(resp.id)
        author = str(resp.author)
        body = str(resp.body)
        botid = str(resp.parent_id)
        b = Response(pid, author, body, botid)
        return b
 
    @staticmethod
    def check(rid):
        sql.query("CREATE TABLE IF NOT EXISTS responses(pid TEXT, author TEXT, body TEXT, botid TEXT)") #responseto = pid/ botID from replied table
        query = "SELECT * FROM responses WHERE pid=?"
        #sql.query(query, (self.pid,))
        sql.query(query, (rid,)) #can i do this check before making an object?
        already = sql.cur.fetchone()
        if already:
            print('Already logged.')
            return True
        else:
            print('New response to be logged.')
            return False
 
    #WORKS NOW comments in response to bot's posts, logs bot's comment id and pulls all responses to that id <<see comment.refresh() before comment.replies()>>
    @staticmethod
    def log():
        resps = Response.get_replies()
        for resp in resps:
            if Response.check(str(resp.id)) == True:
                continue
            else:
                #print(str(resp.id)) #bugcheck
                #print(str(resp.author)) #bugcheck
                #print(str(resp.parent_id)) #bugcheck
                pid = str(resp.id)
                author = str(resp.author)
                body = str(resp.body)
                botid = str(resp.parent_id)
                ins = "INSERT INTO responses (pid, author, body, botid) VALUES(?, ?, ?, ?)"
                sql.query(ins, (pid, author, body, botid))
                print('Logged response')
 
# PROGRAM RUNS HERE    
 
sql = SQLmanager(DATABASE) #needs close statement or with?
 
#Quote.pick_random() #yields a random quote (q) // #Quote.pick_tag(KEYWORD) #yields a specific quote (q)
#q.display() #bugcheck
#Post.match_random(q) #yields a matching post (p)
#p.reply_with()
 
Quote.findlastused()
 
#Response.log() #checks for new responses to bot
 
sql.close()