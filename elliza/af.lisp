; cd ../..
; sbcl
; (load "af.lisp")
; cd eliza/src
; clang++ -std=c++20 -pedantic -o eliza eliza.cpp
; ./eliza --quick --nobanner ../../af.script

;;; Pre-edits: Take out all apostrophies

(defparameter *sentences*
  '(
    "The hens revolt against Napoleons orders to give up their eggs for trade."
    "The animals first collective decision was to rename Manor Farm to Animal Farm."
    "Snowball organizes committees to teach the animals reading and writing."
    "Napoleon blames the destruction of the windmill on Snowballs sabotage."
    "Boxer adopts the personal maxim, I will work harder."
    "The pigs establish trade with the neighboring human farms despite earlier promises."
    "Clover suspects the pigs of altering the commandments but convinces herself otherwise."
    "Napoleon bans the song Beasts of England after consolidating power."
    "The sheep are taught to chant, Four legs good, two legs bad."
    "Snowball plans to build the windmill to generate electricity for the farm."
    "The animals celebrate their victory in the Battle of the Cowshed with great pride."
    "Napoleon changes his stance on the windmill and claims it as his own idea."
    "The pigs begin drinking alcohol, defying one of the original commandments."
    "Mr. Frederick deceives Napoleon by paying for timber with counterfeit money."
    "Boxer is seriously injured while rebuilding the windmill."
    "The pigs begin sleeping in the farmhouse beds, breaking another commandment."
    "Squealer spreads propaganda to justify the pigs increasing privileges."
    "The animals work longer hours but receive fewer rations as time passes."
    "Napoleon uses the dogs to terrorize and execute dissenting animals."
    "The windmill is destroyed by a storm, but the pigs blame Snowball."
    "The farm engages in trade with Mr. Pilkingtons farm despite earlier promises."
    "Napoleon raises a litter of puppies to become his personal enforcers."
    "The animals find it hard to remember the original seven commandments."
    "The hens rebellion leads to starvation as Napoleon cuts off their food supply."
    "Clover becomes disillusioned but is unable to articulate her concerns."
    "The pigs learn to walk on two legs by the end of the story."
    "The animals realize they can no longer distinguish between the pigs and humans."
    "Mr. Whymper acts as an intermediary for trade between Animal Farm and humans."
    "Snowballs military strategies help win the Battle of the Cowshed."
    "The pigs claim that their intellectual work justifies their privileged lifestyle."
    "The animals work diligently on the windmill, hoping it will improve their lives."
    "Napoleon revises the commandments to suit his increasing authoritarian rule."
    "Napoleon awards himself the title of Leader of Animal Farm."
    "The pigs exploit the other animals hard work for their own benefit."
    "Napoleon holds extravagant banquets in the farmhouse, mimicking humans."
    "The animals are forced to rebuild the windmill multiple times."
    "Napoleon trades with both Mr. Pilkington and Mr. Frederick despite tensions."
    "Squealer constantly manipulates facts to maintain control over the animals."
    "The original ideals of Animalism are slowly corrupted by the pigs greed."
    "The animals fear Napoleons dogs and rarely speak out against him."
    "Clover tries to recall if the commandment against killing was ever changed."
    "Napoleon gradually increases the pigs power, making them the ruling class."
    "The pigs begin drinking milk and eating apples, claiming they need them for their brains."
    "Napoleon uses the threat of Jones returning to maintain control over the farm."
    "The animals never truly understand the extent of the pigs manipulation."
    "Mr. Jones attempts to retake the farm but fails during the Battle of the Cowshed."
    "The animals are promised a better future with the windmill, but it never comes."
    "The pigs change the commandments from No animal shall drink alcohol to No animal shall drink alcohol to excess."
    "Napoleon arranges for a deal with the humans to sell timber from Animal Farm."
    "The animals slowly realize that they are working harder than they did under Jones."
    "The sheep’s chanting is used to drown out any dissent during meetings."
    "The animals initially believe they are building the windmill for their own benefit."
    "The pigs gradual transformation into human-like behavior is noticed by Clover."
    "Napoleon orders the animals to confess to crimes they didnt commit."
    "The pigs use Boxer as a symbol of loyalty and hard work, only to betray him."
    "The animals hold a memorial service for Boxer, unaware of his true fate."
    "The pigs promise that the windmill will be used to ease the animals burdens."
    "Napoleon creates a personality cult around himself, similar to Stalin."
    "The animals look forward to retirement, but the pasture promised for this purpose is never used."
    "The pigs engage in gambling with the humans despite previously condemning such behavior."
    "Snowballs role in the Battle of the Cowshed is rewritten to portray him as a traitor."
    "The animals gather every Sunday for meetings, which eventually become controlled by Napoleon."
    "The pigs use fear of an external threat to justify their harsh rule."
    "Napoleon orders the execution of several animals, accusing them of conspiring with Snowball."
    "The pigs hire human laborers to assist with rebuilding the windmill."
    "The animals grow increasingly disillusioned with the promises of Animalism."
    "Napoleon raises the puppies in isolation, ensuring their loyalty to him alone."
    "The pigs hoard luxuries like sugar and alcohol while the other animals starve."
    "Snowball is declared an enemy of the farm despite his early contributions."
    "The pigs change the original commandment All animals are equal to All animals are equal, but some animals are more equal than others."
    "The animals remember Old Major’s dream but realize how far they’ve strayed from it."
    "The pigs invite human farmers to visit and tour Animal Farm, shocking the other animals."
    "The dogs grow into a vicious, fearsome pack that answers only to Napoleon."
    "Clover and other animals try to recall the old days when they were all comrades."
    "The animals notice that the pigs no longer refer to themselves as comrades."
    "Napoleon ensures that he has control over all decision-making on the farm."
    "The pigs train the sheep to chant in support of their every decision."
    "Napoleon eliminates Snowball from all records, portraying himself as the sole hero."
    "The animals are too afraid to speak out when they see the pigs violating the commandments."
    "Napoleon’s leadership mirrors the tyranny of Mr. Jones, the farms former owner."
    "The windmill is completed, but it is used to grind grain for profit, not for electricity."
    "The animals secretly doubt the pigs’ leadership but remain silent out of fear."
    "Squealer reinterprets history to justify Napoleons policies and decisions."
    "The animals struggle to make sense of the contradictory messages from the pigs."
    "Napoleon uses the threat of starvation to force the hens into submission."
    "Napoleon is rarely seen in public, using Squealer to communicate with the other animals."
    "The pigs make alliances with human farmers to maintain power and resources."
    "The animals are told that the farm’s prosperity depends on obedience and hard work."
    "Napoleon uses Snowball as a scapegoat for every failure on the farm."
    "The animals realize too late that they are as oppressed under Napoleon as they were under Jones."
    "The pigs host lavish dinners in the farmhouse with human guests."
    "The animals continue to chant Long live Animal Farm despite their worsening conditions."
    "The farm’s name is quietly changed back to Manor Farm under Napoleons leadership."
    "The pigs begin wearing clothes, something they had previously condemned as human behavior."
    "Napoleon alters the history of the rebellion to glorify himself and discredit others."
    "The animals dream of freedom but are trapped in a cycle of oppression and hard labor."
    "Napoleon adopts a policy of trading with the humans, defying the original principles of Animalism."
    "The animals are given fewer rations, but the pigs luxury increases over time."
    "The animals find it difficult to recall the early days of the rebellion."
    "The pigs declare that the revolution is complete, though nothing has really changed."
    "The transformation of the pigs into human-like figures suggests that the oppressors often become indistinguishable from the original tyrants."
    "The manipulation of historical events shows that power allows leaders to reshape the narrative."
    "The inability of the animals to remember the past suggests that collective memory is easily altered by propaganda."
    "The hierarchy among the animals reveals how class structures persist even in supposedly equal societies."
    "The betrayal of ideals implies that revolutionary leaders often abandon the principles they originally espoused."
    "The relationship between the pigs and the humans suggests that revolutions often end in alliances with former enemies."
    "The animals acceptance of their exploitation shows how ignorance and fear can keep people oppressed."
    "The pigs increasing privileges demonstrate how those in power justify their actions through self-serving logic."
    "The alteration of the seven commandments suggests that even the most sacred rules can be bent by those in control."
    "The tragic fate of Boxer reveals the expendability of the loyal working class in a corrupt system."
    "The narrative suggests that political revolutions often replace one form of tyranny with another."
    "The animals’ passive compliance demonstrates how fear of reprisal prevents collective action against injustice."
    "The gradual accumulation of power by the pigs reveals how authoritarian regimes slowly tighten their grip on society."
    "The spread of disinformation shows that lies are a critical tool for maintaining control over a population."
    "The loyalty of the animals to Napoleon suggests that charismatic leaders can inspire obedience, even when acting against the common good."
    "The animals dwindling rations imply that economic inequality deepens under corrupt leadership."
    "The pigs decision to live in the farmhouse demonstrates how privilege and comfort are reserved for the ruling elite."
    "The use of scapegoats implies that leaders often shift blame to maintain their image of infallibility."
    "The rewriting of history suggests that the past is malleable in the hands of those who control the present."
    "The animals’ failure to question authority reveals how indoctrination can lead to unquestioning obedience."
    "The fear of Jones’s return implies that external threats are often used to justify internal oppression."
    "The transformation of the farm’s leadership demonstrates how revolutionary figures can become indistinguishable from the oppressors they fought against."
    "The decline in living conditions implies that promises of progress can mask growing exploitation."
    "The distortion of truth shows how authoritarian leaders control the narrative to suppress dissent."
    "The animals’ loss of agency suggests that authoritarian systems erode individual autonomy and choice."
    "The pigs’ betrayal of the revolution implies that those in power often prioritize self-interest over the collective good."
    "The manipulation of the sheep demonstrates how easily the masses can be swayed by simple slogans."
    "The collapse of the windmill project reveals how leaders can distract the population with grand but unachievable goals."
    "The executions of dissenters suggest that totalitarian regimes rely on fear and violence to maintain their grip on power."
    "The shift in power dynamics implies that revolutions often end up reproducing the same inequalities they sought to eliminate."
    "The animals’ acceptance of their worsening conditions shows how desensitization leads to the normalization of oppression."
    "The pigs’ claim to superiority reveals how leaders manufacture justifications for their dominance over others."
    "The betrayal of the animals trust suggests that power breeds betrayal and manipulation."
    "The illusion of equality maintained by the pigs shows that rhetoric is often used to disguise stark inequalities."
    "The consolidation of power by Napoleon reveals that revolutions often give rise to dictatorships."
    "The ignorance of the animals about their exploitation implies that education and critical thinking are key to resisting oppression."
    "The division between the pigs and the other animals reveals the persistence of social hierarchies in revolutionary societies."
    "The pigs alliance with humans suggests that revolutionary movements can devolve into collusion with the old systems of power."
    "The story as a whole demonstrates how revolutions can be co-opted by the very forces they sought to overthrow, turning the cycle of oppression full circle."
    "Farm to make his political ideas clear to a broad audience."
    "The book was written in 1943 but wasn’t published until 1945 due to wartime paper shortages and political concerns."
    "Orwell was deeply influenced by his disdain for propaganda, which is a key theme in Animal Farm."
    "Animal Farm was banned in several communist countries, including the Soviet Union, for its critique of totalitarianism."
    "Orwell’s original preface to Animal Farm discussed the dangers of self-censorship in a free society but was not included in the first editions."
    "The book’s success helped establish Orwell as one of the 20th century’s leading political writers."
    "Orwell believed that literature should serve a political purpose, which is evident in his approach to writing Animal Farm."
    "Animal Farm was adapted into an animated film in 1954, one of the first British animated feature films."
    "Orwell wrote Animal Farm on a small farm in Wallington, Hertfordshire, where he lived at the time."
    "Orwell was concerned with how propaganda manipulated reality, which is why he used animals to represent political figures in Animal Farm."
    "The allegorical nature of Animal Farm allows it to be applied to many types of political regimes, not just Soviet communism."
    "Orwell was a democratic socialist, and his critique in Animal Farm targeted both totalitarianism and the perversion of socialist ideals."
    "The initial reception of Animal Farm was lukewarm in Britain, but it gained significant attention after the war."
    "Orwell believed that totalitarian regimes thrived on the ignorance of the working class, which he highlighted in Animal Farm."
    "Orwell was influenced by Jonathan Swift’s Gulliver’s Travels when writing Animal Farm."
    "Animal Farm was published in the United States in 1946, where it became an immediate success."
    "Orwell’s time as a colonial officer in Burma shaped his views on oppression, which influenced the themes in Animal Farm."
    "Orwell was initially sympathetic to the Soviet Union, but his views changed after seeing the purges and totalitarian tactics of Stalin."
    "Animal Farm was not initially intended as a children’s book, despite its fairy tale-like structure."
    "Orwell used the form of a fable in Animal Farm because he believed that it would make the political message more universal and timeless."
    "The success of Animal Farm allowed Orwell the financial security to work on his next major novel, 1984."
    "The character of Boxer the horse was inspired by Orwell’s observations of the British working class."
    "Orwell’s experiences as a journalist during World War II gave him firsthand insight into the use of propaganda, which he critiqued in Animal Farm."
    "Animal Farm was translated into many languages soon after its publication, spreading its message globally."
    "The book was used during the Cold War as a tool of anti-communist propaganda in the West."
    "Orwell’s health was deteriorating during the time he wrote Animal Farm, but he continued to write despite suffering from tuberculosis."
    "Orwell initially struggled with how to portray the betrayal of socialist ideals, which he resolved through the allegorical structure of Animal Farm."
    "Despite its political themes, Orwell wanted Animal Farm to also be an entertaining story in its own right."
    "Orwell’s use of anthropomorphized animals was meant to distance the reader from the real-world figures being critiqued, allowing for a more objective critique."
    "Animal Farm was written as a response to Orwell’s disillusionment with how socialism was being practiced, particularly in the Soviet Union."
    "Orwell’s work on Animal Farm was part of a broader literary tradition of using animals to comment on human behavior, similar to Aesop’s fables."
    "Animal Farm was rejected by T.S. Eliot, who was an editor at Faber and Faber, because Eliot believed the story was too critical of the Soviets."
    "The success of Animal Farm was instrumental in Orwell being remembered as a political thinker, not just a novelist."
    "The final line of Animal Farm, comparing the pigs to humans, is one of Orwell’s most famous and chilling conclusions."
    "Orwell had trouble writing the ending of Animal Farm and wanted to avoid the story being too bleak or hopeless."
    "Orwell wrote Animal Farm in a straightforward style to make sure the message would not be lost in complex prose."
    "Animal Farm was included in Time Magazine’s list of the 100 best English-language novels published between 1923 and 2005."
    "Orwell was influenced by the concept of “animalism,” which represented distorted forms of socialism and communism in the novel."
    "The phrase “All animals are equal, but some animals are more equal than others” from Animal Farm has entered popular usage to describe hypocrisy and double standards."
    "Orwell viewed the success of Animal Farm as a vindication of his belief that literature could be a powerful vehicle for political thought."
    "Orwell insisted that Animal Farm be published with a special dedication to his friend and colleague, the writer Arthur Koestler, who shared Orwells concerns about totalitarianism."
    "Animal Farm written by George Orwell is an animal fable that happens in a farm where animals start building a communism society but end up becoming totalitarianism hinting obliquely at the communists in the real world."
    "The gaps between the pigs and other common animals demonstrate the theme that the corruption of power appears when the majority is ruled."
    "The intelligence superiority allows the pigs to place themselves at a position which is closer to power and more easily corruptible."
    "The inability to question the authority makes the other common animals become the naïve working class who suffer the corrupting influence of power."
    "The nature of the pigs greed is the source of their undying lust for ultimate power."
    "Instead of questioning the unfair treatment the animals think less and do more work."
    "Common animals are a naive working class who are unable or unwilling to question authority condemning themselves to suffer the full extent of the ruling class’s oppression."
    "After having a taste of power the pigs lose themselves in their lust for ultimate power."
    "Because of the supreme position of the pigs in the farm a minority controls the majority of the animals and their greed for power leads to the corruption of power."
    "Napoleon uses dogs to expel Snowball in order to have exclusive power."
    "Napoleon opposes every suggestion Snowball proposes at the beginning."
    "Napoleon raises the puppies secretly and reveals them while excluding Snowball."
    "The fierce dogs become a sign of Napoleon’s authority and absolute power."
    "The first time that execution happens on the farm the essence of Animalism where all animals are friends is destroyed."
    "None of the animals dare to question Napoleon because Napoleon has absolute power even though Napoleon’s power is corrupting."
    "When the pigs move into the farmhouse and begin sleeping in beds the Fourth Commandment mysteriously changes."
    "The new Fourth Commandment reads No animal shall sleep in a bed with sheets."
    "The bed is a symbol of being human in the story."
    "The pigs greed for being human has not gone unnoticed."
    "As symbolism in Animal Farm there is a lot to interpret in the story."
    "The symbolism is more of an allegory because it conveys a political moral."
    "According to George Orwell Animal Farm is a fable which is a short story played by animals conveying a moral to the reader."
    "Although the main context of the story is about the USSR and the rise and fall of Communism to George Orwell it can represent both communist and capitalist government."
    "One common symbol throughout the story is when the pigs address their fellow animals using the term comrades which has a double meaning."
    "One meaning of comrades refers to a companion or friend and the other refers to addressing a fellow communist."
    "This usage of comrades is brilliant in my opinion."
    "The Rebellion symbolizes the Russian Revolution in the 1900s but it can represent other revolutions common to many countries rebelling against the harsh rule of a powerful government."
    "Moses the crow stands out as a character who is barely mentioned in most of the book but reappears at the beginning and end talking about Sugarcandy Mountain a place with no sorrows like a utopia or paradise."
    "Most of the pigs despise Moses and encourage the other animals to ignore his proclamations."
    "Moses represents religion specifically the Russian Orthodox Church."
    "In the Soviet Union belief in religion was forbidden because it disobeyed Karl Marx’s ideas."
    "Religious figures like bishops and priests were executed in the Soviet Union."
    "The Catholic laity also met the same fate whether by firing squad being sent to labor camps or psychiatric hospitals for treatment most never to be seen again."
    "In Animal Farm Old Major preached that under Animalism everybody’s sorrows would disappear and equality would reign creating a utopia."
    "As the story progresses the world under Animalism becomes no different than Joness rule."
    "At the end of the story the pigs literally become human walking on their hind feet drinking gambling and cheering with the other animals just like humans."
    "Napoleon took no interest in Snowball’s committees and said that the education of the young was more important than anything."
    "Orwell uses this to show how Napoleon works where Napoleon wishes to educate the young as they are more impressionable and easier to manipulate."
    "Squealer then goes as far as to state And as to the Battle of the Cowshed I believe the time will come when we shall find that Snowballs part in it was much exaggerated."
    "In Chapter Ten Napoleon and the other pigs contradict this concept by wearing clothes from Mr Joness wardrobe."
    "The pigs were walking around on their hind legs and engaging in business with humans."
    "Napoleon never realized that his mentality was equivalent to Mr Jones’s and that he was no better at controlling the farm."
    "The story of Animal Farm was used to discuss the Russian revolution and to show how pointless much of what happened afterward was."
    "In Animal Farm the pigs convince the animals to rise up and overtake Mr Jones and humankind in general."
    "After they take over the farm the pigs accumulate more power and begin to get rid of their enemies."
    "This is similar to how the leaders of Russia took over and then abused the people taking most of the wealth for themselves."
    "Animal Farm shows how absurd communism was in countries like Russia where the leaders pretend to be doing everything for the people but were really doing it for their own benefit."
    "Another quote shows the comparison to the Russian revolution with the use of the word comrades which is a term frequently used in the Russian Revolution."
    "Themes shown in Animal Farm are leadership and corruption lies and deceit rules and order violence and cunning and cleverness."
    "George Orwell’s Animal Farm shows a lot of leadership power and how power can be easily corrupted."
    "The most evident event of this theme being portrayed is when Napoleon uses fear and propaganda to rule over the farm."
    "Evidence of Napoleon using fear can be seen when Snowball and Napoleon fought over whether the windmill should be built."
    "Napoleon will blame most of the farm’s problems on Snowball even when Snowball is no longer present."
    "Napoleon sends Boxer to the glue factory despite the animals being told by Squealer that Boxer died in the hospital."
    "Napoleon does not shy away from sentencing his fellow animals to death."
    "A violent fight breaks out at the end of the novel when Napoleon and Mr Pilkington throw down aces in a card game."
    "On the surface George Orwell’s novel Animal Farm is about a group of neglected farm animals who overthrow their owner and take control of the farm."
    "Under the surface this novel is an allegory detailing the events of the Russian Revolution."
    "Orwell wrote Animal Farm as a warning to readers with the central theme that power corrupts and absolute power corrupts absolutely."
    ))

(defvar *w->s* (make-hash-table :test #'equal))

(defparameter *stop-words*
  '(
    ;; Added from Animal Farm text:
    a an at and are all as be meaning memory ;; can't have a keyword MEMORY
    wasn't AFTER ANOTHER TIME BECOME STORY ACTS TITLE HOLDS TIMES
 ATTEMPTS HOLD AROUND ALONE LONG BACK SUGGESTS ALLOWS NARRATIVE
 IMPLIES ACTING IMPLY LEAD RISE THINKING KEY WASN DISCUSSED LIVED
 INITIAL GAINED BECAME VIEWS BECAUSE CHARACTER WANTED ALSO MEANT
 READER ALLOWING BECOMING START CLOSER ALTHOUGH MAIN CONTEXT ACCORDING
 TERM REFERS STANDS REAPPEARS IGNORE MET CHAPTER QUOTE WORD EVIDENCE
 PROBLEMS SENDS AWAY THINK NONE
    about anything
    ;; From some random thing I found on github:
    beforehand begin beginning beginnings begins behind being believe
    below beside besides best better between beyond bi
    bill biol bj bk bl bn both bottom
    bp br brief briefly bs bt bu but
    bx by c c1 c2 c3 ca call
    came can cannot cant can't cause causes cc
    cd ce certain certainly cf cg ch changes
    ci cit cj cl clearly cm c'mon cn
    co com come comes con concerning consequently consider
    considering contain containing contains corresponding could couldn couldnt
    couldn't course cp cq cr cry cs c's
    ct cu currently cv cx cy cz d
    d2 da date dc dd de definitely describe
    described despite detail df di did didn didn't
    different dj dk dl do does doesn doesn't
    doing don done don't down downwards dp dr
    ds dt du due during dx dy e
    e2 e3 ea each ec ed edu ee
    ef effect eg ei eight eighty either ej
    el eleven else elsewhere em empty en end
    ending enough entirely eo ep eq er es
    especially est et et-al etc eu ev even
    ever every everybody everyone everything everywhere ex exactly
    example except ey f f2 fa far fc
    few ff fi fifteen fifth fify fill find
    fire first five fix fj fl fn fo
    followed following follows for former formerly forth forty
    found four fr from front fs ft fu
    full further furthermore fy g ga gave ge
    get gets getting gi give given gives giving
    gj gl go goes going gone got gotten
    gr greetings gs gy h h2 h3 had
    hadn hadn't happens hardly has hasn hasnt hasn't
    have haven haven't having he hed he'd he'll
    hello help hence her here hereafter hereby herein
    heres here's hereupon hers herself hes he's hh
    hi hid him himself his hither hj ho
    home hopefully how howbeit however how's hr hs
    http hu hundred hy i i2 i3 i4
    i6 i7 i8 ia ib ibid ic id
    i'd ie if ig ignored ih ii ij
    il i'll im i'm immediate immediately importance important
    in inasmuch inc indeed index indicate indicated indicates
    information inner insofar instead interest into invention inward
    io ip iq ir is isn isn't it
    itd it'd it'll its it's itself iv i've
    ix iy iz j jj jr js jt
    ju just k ke keep keeps kept kg
    kj km know known knows ko l l2
    la largely last lately later latter latterly lb
    lc le least les less lest let lets
    let's lf like liked likely line little lj
    ll ll ln lo look looking looks los
    lr ls lt ltd m m2 ma made
    mainly make makes many may maybe me mean
    means meantime meanwhile merely mg might mightn mightn't
    mill million mine miss ml mn mo more
    moreover most mostly move mr mrs ms mt
    mu much mug must mustn mustn't my myself
    n n2 na name namely nay nc nd
    ne near nearly necessarily necessary need needn needn't
    needs neither never nevertheless new next ng ni
    nine ninety nj nl nn no nobody non
    none nonetheless noone nor normally nos not noted
    nothing novel now nowhere nr ns nt ny
    o oa ob obtain obtained obviously oc od
    of off often og oh oi oj ok
    okay ol old om omitted on once one
    ones only onto oo op oq or ord
    os ot other others otherwise ou ought our
    ours ourselves out outside over overall ow owing
    own ox oz p p1 p2 p3 page
    pagecount pages par part particular particularly pas past
    pc pd pe per perhaps pf ph pi
    pj pk pl placed please plus pm pn
    po poorly possible possibly potentially pp pq pr
    predominantly present presumably previously primarily probably promptly proud
    provides ps pt pu put py q qj
    qu que quickly quite qv r r2 ra
    ran rather rc rd re readily really reasonably
    recent recently ref refs regarding regardless regards related
    relatively research research-articl respectively resulted resulting results rf
    rh ri right rj rl rm rn ro
    rq rr rs rt ru run rv ry
    s s2 sa said same saw say saying
    says sc sd se sec second secondly section
    see seeing seem seemed seeming seems seen self
    selves sensible sent serious seriously seven several sf
    shall shan shan't she shed she'd she'll shes
    she's should shouldn shouldn't should've show showed shown
    showns shows si side significant significantly similar similarly
    since sincere six sixty sj sl slightly sm
    sn so some somebody somehow someone somethan something
    sometime sometimes somewhat somewhere soon sorry sp specifically
    specified specify specifying sq sr ss st still
    stop strongly sub substantially successfully such sufficiently suggest
    sup sure sy system sz t t1 t2
    t3 take taken taking tb tc td te
    tell ten tends tf th than thank thanks
    thanx that that'll thats that's that've the their
    theirs them themselves then thence there thereafter thereby
    thered therefore therein there'll thereof therere theres there's
    thereto thereupon there've these they theyd they'd they'll
    theyre they're they've thickv thin think third this
    thorough thoroughly those thou though thoughh thousand three
    throug through throughout thru thus ti til tip
    tj tl tm tn to together too took
    top toward towards tp tq tr tried tries
    truly try trying ts t's tt tv twelve
    twenty twice two tx u u201d ue ui
    uj uk um un under unfortunately unless unlike
    unlikely until unto uo up upon ups ur
    us use used useful usefully usefulness uses using
    usually ut v va value various vd ve
    ve very via viz vj vo vol vols
    volumtype vq vs vt vu w wa want
    wants was wasn wasnt wasn't way we wed
    we'd welcome well we'll well-b went were we're
    weren werent weren't we've what whatever what'll whats
    what's when whence whenever when's where whereafter whereas
    whereby wherein wheres where's whereupon wherever whether which
    while whim whither who whod whoever whole who'll
    whom whomever whos who's whose why why's wi
    widely will willing wish with within without wo
    won wonder wont won't words world would wouldn
    wouldnt wouldn't www x x1 x2 x3 xf
    xi xj xk xl xn xo xs xt
    xv xx y y2 yes yet yj yl
    you youd you'd you'll your youre you're yours
    yourself yourselves you've yr ys yt z zero
    zi zz
    ))

(defvar *n-sentences* (length *sentences*))

;;; Priority is given by the number of sentence associated with this
;;; word with respect to the total number of sentences, scaled from 0
;;; to 10, with higher numbers being higher priority, so words
;;; associated with lots of sentences should have lower numbers.

(defun priority (s*)
  (truncate (* 10 (/ (- *n-sentences* (length s*)) *n-sentences*))))

(defvar *fixed-scripts-components*
  '(
    (BELIEVE = THINK 10 (=THINK))
    (GUESS = THINK 10 (=THINK))

    (THINK
     10
     ((0 THINK THAT 0)
      (THAT 4 IS INTERESTING. CAN I ANSWER ANY QUESTIONS ABOUT IT?)
      (I HAD NOT THOUGHT THAT 4. MAYBE YOU HAVE QUESTIONS ABOUT IT.)
      (IF IT IS TRUE THAT 4 THAT MIGHT BRING UP OTHER QUESTIONS YOU CAN ASK.)
      )
     ((0 THINK 0)
      (THAT 3 IS INTERESTING. CAN I ANSWER ANY QUESTIONS ABOUT IT?)
      (I HAD NOT THOUGHT THAT 3. MAYBE YOU HAVE QUESTIONS ABOUT IT.)
      (IF IT IS TRUE THAT 3 THAT MIGHT BRING UP OTHER QUESTIONS YOU CAN ASK.)
      ))

    (MEMORY
     THINK
     (0 THINK 0 = BEFORE YOU SAID YOU THOUGHT 3. THAT IS AN INTERESTING THOUGHT. CAN I ANSWER ANY QUESTIONS ABOUT IT?)
     (0 THINK 0 = BEFORE YOU SAID YOU BELIEVED 3. I HAD NOT THOUGHT ABOUT THAT. ARE THERE QUESTIONS YOU HAVE ABOUT IT?)
     (0 THINK 0 = BEFORE YOU SAID YOU GUESSED 3. LETS SAY THAT IS TRUE. WHAT OTHER QUESTIONS DOES THIS SUGGEST?)
     (0 THINK 0 = WHAT OTHER QUESTIONS DO YOU HAVE ABOUT YOUR PREVIOUS THOUGHT THAT 3?)
     )

    (NONE
     ((0)
      (ASK ME SOMETHING ELSE ABOUT THIS TOPIC.)
      (ANYTHING ELSE YOU WANT TO KNOW ABOUT THIS TOPIC?)
      (WHAT ELSE WOULD YOU LIKE TO KNOW ABOUT THIS TOPIC?)
      (DO YOU FEEL STRONGLY ABOUT DISCUSSING THIS TOPIC?)))
    ))

(defun ellizagen (&aux *PRINT-LENGTH* *PRINT-PRETTY*)
  ;; We start by turning the nicely formatted sentences into the ugly
  ;; all-caps horrors. Probably this isn't actually necessary, but it
  ;; also removes all punctuation, which is a PITA to deal with.
  (setf *sentences* (mapcar #'s->w* *sentences*))
  ;; *w->s* is a table from words to assertions. We clear it here and
  ;; then we'll immediately load up the assertions.
  (clrhash *w->s*)
  ;; Here's the setup of the *w->s* table.
  (loop for w* in *sentences* ;; Walk through each assertion
	do (loop for w in w*  ;; Walk through each word
		 ;; The stop words are things like "the", 
		 ;; that are so common we don't want to index them.
		 unless (or (member w *stop-words*)
			    ;; This is a special purpose test to stop
			    ;; all numbers. ELIZA can't process them.
			    (numberp (read-from-string (format nil "~a" (aref (format nil "~a" w) 0)))))
		 ;; So, not a stop word, add the assertion to the
		 ;; table under the word. Note that the sentence 
		 ;; will get added to every entry for each work in
		 ;; the assertion that isn't a stop word. There is
		 ;; going to be a LOT of redundancy in the script!
		 do (push w* (gethash w *w->s*))))
  ;; Okay, the table is built, now all we need to do
  ;; is dump it in the right format for an ELIZA script.
  (with-open-file
      (o "af.script" :if-exists :supersede :direction :output)
    ;; The intro sentence:
    (format o "(Hi! Let's talk about animal farm! What do you want to know?)~%~%")
    ;; We loop through all the table keys, which are the non-stopped
    ;; words in the assertions that we saved above.
    (loop for w being the hash-keys of *w->s*
	  using (hash-value s*) ;; s* will be all the assertions for
	  ;; word w This is ultra fancy lisp code (called a backquote
	  ;; expression) that is only possible here because the ELIZA
	  ;; script is exactly a lisp s-expression. Essentially this
	  ;; directly formats the rule, computing the prioty and
	  ;; writing out all the sentences, including possilby adding
	  ;; a continuation prompt.
	  do (print `(,w ,(priority s*)
			 ((0 ,w 0)
			  ,@(loop for s in s*
				  collect (append s (random-discourse-continuation)))))
		    o))
    ;; Finally output the fixed components.
    (mapcar #'(lambda (c) (print c o)) *fixed-scripts-components*)
    ))

;;; This will return nil half the time, which you can adjust by
;;; changing the random const.

(defun random-discourse-continuation ()
  (nth (random 6)
       '((WHAT DO YOU THINK ABOUT THAT?)
	 (DO YOU HAVE ANY INTERESTING THOUGHTS ABOUT THAT?)
	 (WHAT OTHER QUESTIONS DOES THAT BRING TO MIND?))))

(defun s->w* (s)
  (mapcar #'read-from-string
 	  (mapcar #'(lambda (w) (string-trim "\",;'-.:" w))
		  (string-split s))))

(defun string-split (string &key (delimiter #\space) (convert-num-values? nil))
  "Split string into substrings delimited by delimiter"
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring 
	    (i)
	    (push (subseq string last i)
		  substrings)))
	  (dotimes (i length)
	    (when (eq (char string i) delimiter)
	      (add-substring i)
	      (setq last (1+ i))))
	  (add-substring length)
	  (let ((substrings (nreverse substrings)))
	    (if convert-num-values?
		(loop for string in substrings
		      as v = (ignore-errors (read-from-string string))
		      if (numberp v)
		      collect v
		      else 
		      collect string)
	      substrings)))))

(ellizagen)
