Source: http://blog.everythings-beta.com/?p=108
Accessed: 20120311
Author: srijak
Date written: 20081224


#!/usr/bin/python
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#For a copy of the license see .
 
# A simple eliza bot in python
#
import re
import random
 
class Agent:
    def __init__(self,brain):
        self.patternResponses = []
        self.defaultResponses = []
        self.Usersname =''
        self.Usernametag ='Username'
        self.defRespIndex =0
        self.pronounMap = {
                           #generic pronouns
                           'i':'you','you':'i',
                           'my':'your', 'me':'you',
                           'your':'my',
                           "i've":"you've","we've":"they've",
                           # "you've":"i've","they've":"we've",
                           #some names, these wll get applied if the name
                           #isnt the current user's name
                           'fred':'he', 'jack':'he',
                           'jane':'she',
                           }
 
        self.change_mind(brain)
 
    def change_mind(self,brain):
        """
        Change mind on the fly.
        To change the Agent's brain
        """
        assert (len(brain) == 2)
        self.patternResponses = brain[0]
        self.defaultResponses = brain[1]
        print "Brain initialized."
 
 
    def tell(self,input):
        sentence = input.lower().strip().replace('(','').replace(')','').split(' ')
        bl = []
        matchedPat = []
        for pat in self.patternResponses:
            bl = self.pattern_match(pat[0],sentence,[])
            if bl == ['FAIL']:
                continue
            matchedPat = pat
            break
        if bl == ['FAIL']:
            #choose from a default response
            # or just append a question mark on the
            # user's sentence.
            print '... No pattern Response pair selected.'
            print '... Choosing a default response.'
 
            #Used to be randon using random.choice
            #but then i noticed that the requirements say that
            #the same default response may not be used again
            #before every other one has been used.
            self.defRespIndex = self.defRespIndex +1
 
            if  self.defRespIndex >= len(self.defaultResponses):
                #reset the counter to 0
                self.defRespIndex = 0 ;
                #Possible improvement: shuffle the default response array, so that
                # the responses dont come in the same sequence again.
                #last on got chosen, make the user input into
                #a question
                return ' '.join(sentence) + ' ?'
            else:
                return ' '.join(self.defaultResponses[self.defRespIndex])
        else:
            print '... Pattern Response pair selected:', pat
            print '... BL before call to change_pronouns: ',bl
            blAfter = self.change_pronoun(bl)
            print '... BL after call to change_pronouns: ',blAfter
 
 
            rep = self.build_reply(pat[1],blAfter)
            return ' '.join(rep)
 
 
 
    def make_regexp(self,pattern):
        """
        Convert a pattern list into an equivalent
        regular expression
        """
        assert(isinstance(pattern,list))
        r = ['^']  #for exact matches we prepend ^ and $ to the regexp.
        usedBackrefs = []
        for tok in pattern:
            if isinstance(tok,str):
                #WORD
                r.append('(' + tok +')')
            elif isinstance(tok, int):
                # 0 or 1
                if tok == 0 :
                    r.append('[a-zA-Z ]*?') # match 0+ word(chars + space),
 
                elif tok == 1:
                    r.append('[a-zA-Z]+') # match 1 word
                else:
                    raise Exception, "Ints can only be 1 or 0, not:", tok
            elif isinstance(tok, list):
                #bindinglist pattern list
                if tok[0] == 0 :
                    #if we havent used this backref yet, then add ?P<name>
                    #and add it to the used backrefs list.
                    if not tok[1] in usedBackrefs:
                        usedBackrefs.append(tok[1])
                        r.append('(?P<'+tok[1] +'>[a-zA-Z ]*?)')
                    else:
                        #if we have come across his back ref, then add?P=name
                        r.append('(?P='+tok[1]+')')
                elif tok[0] == 1:
                    if not tok[1] in usedBackrefs:
                        r.append('(?P<'+tok[1] +'>[A-Za-z]+)')
                        usedBackrefs.append(tok[1])
                    else:
                        #if we have come across his back ref, then add?P=name
                        r.append('(?P='+tok[1]+')')
                else:
                    raise Exception, "Valid first elems of BL pattern = 1 or 0, not:", tok
 
        #for exact matches we prepend ^ and $ to the regexp.
        #also, we disregard surrounding whitespace.
        ret =  '(\s)*'.join(r) + '$(\s)*'
        return re.compile(ret)
 
    def pattern_match(self,pattern,sentence,bl):
        """
        return the populated binding list if the pattern matches,
        otherwise return the binding list with the element 'FAIL'
        """
        pat = self.make_regexp(pattern)
        s = ' '.join(sentence)
        matches =  re.search(pat,s)
        if (matches == None): # if we dont have a match
            #then return w/o modifying the bindinglist.
            return ['FAIL']
        bindingList = list(bl)
        grps = matches.groups()
        for k in pat.groupindex:
            b=[]
            b.append(k)
            for x in grps[pat.groupindex[k]- 1].split(' '):
                if x.strip() !='':
                    b.append(x)
            bindingList.append(b)
        return bindingList
 
    def change_pronoun(self,bl):
        """
        reverses pronouns in a binding list.
        """
        #if the pronoun dic contains a elemen, then
        #replace it with its corresponding value.
        bindingList = list(bl)
        for i in range(len(bindingList)):
            if isinstance( bindingList[i], list):
                for j in range(len(bindingList[i])):
                    if j > 0:
                        if self.pronounMap.has_key(bindingList[i][j].lower()):
                            bindingList[i][j] = self.pronounMap[bindingList[i][j]]
        return bindingList
    def build_reply(self,respPattern,bindingList):
        """
        build_reply ([[1, 'subject'], 'loves', [0, 'object']],
                    [['subject', 'jane'], ['object', 'ice', 'cream']])
        will return  ['jane', 'loves', 'ice', 'cream'].
        """
        #convert the bindlingList to a dictionary so thats its easy to
        #get the binding label
        bl = {}
        reply = []
        for elem in bindingList:
            #do the username chack firsdt, the id Username is
            #only used in relation to the username .This is
            #so that we can kinda remember the user's name
            if (elem[0] == self.Usernametag):
                # if we already have user name set, then
                # Hi, username , what happend to oldusername
                if self.Usersname !='':
                    tmp =self.Usersname
                    self.Usersname = elem[1]
                    return [elem[1],'eh?','what','ever','happened','to',tmp]
                else:
                    self.Usersname = elem[1]
            if bl.has_key(elem[0]) == elem[1:]:
                raise Exception, 'Binding list has name collision for: ', elem[0]
            else:
                if isinstance(elem,list) and len(elem) > 1:
                    bl[elem[0]] = elem[1:]
                #else:
                #    print elem, 'binding element ',elem,' has no binded things'
        for elem in respPattern:
            if isinstance(elem,list) and len(elem) > 1:
                x = elem[1]
                if bl.has_key(x):
                    for y in bl[x]:
                        reply.append(y)
                #else:
                #    print 'not found in bl:', x
            else: #anything else, just append to the list.
                reply.append(elem)
        return reply
 
 
 
 
def talk(brain):
    assert((isinstance(brain,list)) and (len(brain) ==2 ))
    b = Agent(brain)
 
    while 1:
        try:
            inp = raw_input("> ").lower()
        except EOFError:
            print 'Toodle do,\n you can type (quit) if you want be more pleasant.'
            break
        if len(inp.strip()) > 0 :
            if inp.strip().lower() == 'quit':
                print 'To exit the program, type (quit) [In parentheses]'
            if inp.strip().lower() == '(quit)':
                print 'Bye'
                break
            else:
                print b.tell(inp)
 
 
def start():
    defReplys = [
             ['Is','that','right?'],
             ['Excellent'],
             ['Interesting'],
             ['You','dont','say'],
             ['I','wouldnt','have','guessed']
             ]
 
 
    patternResps = [
    #[[pattern],[response]]
    #Basics
    [[0,'my','name','is',[1,'Username'],0],['pleased','to','meet','you',[1,'Username']]],
    [['hello'],['Good','day']],
    [['how','are','you',[0,'a']],['Good','How','are','you',[0,'a']]],
    [['i','am','fine'],['Glad','to','hear','you','are','fine']],
    [[[1,'a'],'is','fine'],['Glad','to','hear',[1,'a'],'is','fine']],
    [['no'],['Understood']],
    [['yes'],['I','see']],
    [['you','said',[0,'we']],['I may have said ',[0,'we'],'What about it?']],
    [[[1, 'subject'], 'loves', [0, 'object']],['Laudable','why','does',[1,'subject'],'love',[0,'object'],'?']],
    [['because'],['Lack of reason is not a reason']],
    [['i','need',[1,'object']],['why','do','you','need',[0,'object'],'?']],
    [[[0,'subject'],'needs',[0,'object']],['why','does',[0,'subject'],'need',[0,'object'],'?']],
    [['how', 'is','your',[0,'object']],['my',[0,'object'],'is','holding','up','well','.do','you','have','a',[0,'object'],'?']],
    [[[0,'we'],'i','have','a',[0,'thing']],['how','goes', 'it','with','your',[0,'thing'],'?']],
    [[[0,'we'],'i','have',[0,'thing']],['how','is', 'your',[0,'thing'],'?']],
    [[[0,'we'],'is','great'],['how','is',[0,'we'],'great','?']],
    [['i','like',[0,'a']],['Interesting','i','appreciate',[0,'a'],'too']],
    [['i','said',[0,'we']],['Why','did','you','say',[0,'we']]],
    [[0,'you',[1,'b'],'me'],['What','gave','you','the','impression','I',[1,'b'],'you ?']],
    [['i','feel',[0,'a']],['How','long','have','you','felt',[0,'a'],'?']],
   [['tell',[0,'who'],'about',[0,'sub']],['What','would','I','tell',[0,'who'],'about',[0,'sub'],'that','isn\'t','known','?']],
    [[0,'brother',0],['My','brother\'s','name','is','Henry.','Do','you','have','any','siblings?']],
    [[0,'sister',0],['I','dont','have','a','sister.','How','is','your','family','doing?']],
    #anything that ends with exclamation
    [[0,'\!'],['Keep calm']],
 
    # if its a question that hasnt gotten matched uptil now
    # do this. Note: This needs to be last to ensure its the last test
    [[[0, 'question'], '\?'],['What','do','you','mean', [0,'question'],'?']],
 
    ]
 
    ## Run the tests required in the Project specification
    talk([patternResps,defReplys])
 
if __name__=="__main__":
    start()
