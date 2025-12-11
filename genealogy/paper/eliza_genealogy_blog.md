# Reconstructing Program Genealogies: An Update from the ELIZA Phylogeny Project

**Jeff Shrager**  
December 2024

---

## How I Got Here

Let me start with a confession: I've been obsessed with ELIZA for most of my life, though I didn't always know it. When I was 13, back in 1973, I wrote my own version of ELIZA in BASIC. I remember sitting there, so proud of myself for figuring out how to parse strings and flip "I'm afraid of cats" into "How long have you been afraid of cats?" It was the kind of thing that would be trivial for an adult programmer, but to me it was magic—this little piece of quasi-intelligence I'd created.

I had no idea at the time that this would turn into a decades-long quest. I certainly didn't know that my teenage BASIC program would end up being published in Creative Computing magazine in 1977, where it would be typed in by thousands of people with early microcomputers. And I definitely didn't know that if you've ever played with an ELIZA chatbot, there's a decent chance it was actually based on my code—or more accurately, based on someone else's version of my version of Bernie Cosell's version of something that wasn't quite the original ELIZA at all.

Fast forward to around 2000. I'd completely dumped AI and computers, deciding I wanted to become a marine molecular biologist. Screw computers, I thought. I wanted to pipette and grow algae. Of course, you can run but you can't hide from computers—I learned that later. But when I went into biology, I learned about phylogeny, and computational phylogeny, which was just coming up in the years after the human genome was sequenced. In microbiology, phylogeny is incredibly important because bacteria and viruses change really fast. The phylogeny of COVID matters a lot, for instance.

That's when it hit me: what if I could build a phylogenetic tree of ELIZA implementations? If I could measure the changes between all these different versions scattered across the computing landscape, maybe I could map out how they're actually related to each other. Not just historically, but computationally.

But here's the thing: for almost sixty years, nobody actually knew how the original ELIZA worked. Joseph Weizenbaum, who created it at MIT in 1964, never released the code. He published the algorithm, sure, but that was a different era—computer scientists prided themselves on describing algorithms, not "GitHubbing the code" as we'd say now. The code itself sat in a box in MIT's archives, unseen by almost anyone except Weizenbaum and maybe a few of his students.

Then COVID happened. And possibly the only good thing about COVID is that it made remote archival research practical. I reached out to the MIT archives, and a librarian named Myles Crowley got on a Zoom call with me. He pulled Box 8, Folder 1 from Weizenbaum's collection, opened it up, and held it up to his overhead camera. There it was: the original ELIZA code, printed out on that old continuous-feed paper that everyone my age remembers. We could die happy.

But finding the code was just the beginning. Understanding it required assembling a team—what we call "Team ELIZA"—including critical code studies scholars like David Berry and Sarah Ciston, and developers like Anthony Hay who could translate 1960s MAD-SLIP code (not LISP, as everyone thought) into something modern that we could actually run. And that's when things got really interesting.

---

## What This Project Is Really About

This project started with a simple question: how can we reconstruct a genealogy of programs? Not the history as told in manuals or papers or people's recollections, but a genealogy grounded in the artifacts themselves—the code, the structure, the patterns that survive across generations of rewrites, ports, and reimplementations.

Software phylogeny—the project of understanding how earlier programs influence later ones—turns out to be surprisingly elusive. Unlike biological organisms with their (relatively) clear evolutionary pathways, software evolves through multiple mechanisms: direct copying, translation across languages, conceptual reconstruction from descriptions, pedagogical simplification, critical engagement, even deliberate parody. And it's not always clear what it means for one program to be "descended from" another, or even what it means for a program to be "the same" across different implementations.

ELIZA turned out to be the perfect test case for these questions. There are dozens—probably hundreds—of ELIZA-like programs scattered across the computing landscape, implemented in everything from LISP to BASIC to JavaScript to Rust. Some preserve the original architecture faithfully; others simplify it dramatically; still others invoke the name while rejecting the architecture entirely. The ELIZA family comprises a sprawling, poorly documented software "clade" that begs for systematic analysis.

So I decided to treat ELIZA implementations like specimens in a biological study. Over nearly two decades, I've been assembling a corpus at elizagen.org, collecting historical and contemporary versions from archives, repositories, forgotten servers, and personal code collections. And then I asked: can we build an actual phylogenetic tree of these programs?

---

## The Method: LLMs Meet Phylogenetics

The core challenge was this: how do you compare programs written in different languages, using different paradigms, across six decades? You can't just compare source code—a pattern matching system in 1960s SLIP looks nothing like the same logic in modern Python. You need to abstract the programs into some common representation that captures what they actually do.

This is where things get interesting, and where my background in both computer science and cognitive science (yes, I left programming to study psychology at Carnegie Mellon, only to eventually go back to computers, which probably tells you something about me) came in handy.

I used large language models to help extract features from each program. For every implementation in my corpus, I had an LLM identify the salient structural and behavioral properties: Does it use rule-based pattern matching? Does it assign priorities to keywords? Does it have a backtracking pattern matcher? Does it maintain conversational memory? And so on.

The resulting feature lists went into structured YAML files—essentially a "phenotypic" description of each program. But here's the thing: I didn't just trust the LLM output. I manually curated every analysis, adding features the model missed and correcting things it got wrong. And then I did something called "round-robin vocabulary expansion," where I iteratively showed each program's features to the model along with the aggregate vocabulary from all other programs, asking it to identify potentially missing features. This process continued until the vocabulary stabilized.

The final step was encoding these features as binary characters (present or absent) and feeding them into PAUP*, a phylogenetic analysis program used in evolutionary biology. PAUP* uses parsimony methods to construct trees representing relationships among specimens based on shared and divergent features.

---

## The Specimens: A Diverse Corpus

My corpus includes 27 implementations spanning from 1965 to 2025 and eight programming languages. The full list is in the GitHub repository, but let me highlight some of the most interesting specimens:

**The original and its immediate descendants**: Weizenbaum's 1965 MAD-SLIP version (yes, MAD-SLIP, not LISP—that was a surprise), Bernie Cosell's 1966 LISP reconstruction (created from Weizenbaum's verbal description without ever seeing the source code), and several other LISP implementations from the 1960s and 70s.

**The exotic language versions**: A COBOL implementation, a SNOBOL version from 1970, and an Algol version that ran on the Burroughs B5500.

**My own lineage**: My 1973 BASIC version that got published and endlessly copied, spawning countless descendants in the microcomputer era.

**Modern reconstructions**: Anthony Hay's meticulous 2021 C++ version, Peter Norvig's pedagogical LISP implementation from "Paradigms of AI Programming," and various Python and JavaScript versions.

**The outliers**: Sarah Ciston's LadyMouth, a feminist critical art project that invokes ELIZA while rejecting its entire architecture, and a file system utility I threw in as a control specimen—it does pattern matching but has nothing to do with conversational AI.

I also included a pair of byte-for-byte identical clones as a validation test to make sure the method could correctly identify truly identical programs.

---

## What We Found: The Tree Structure

The phylogenetic tree reveals two major patterns, and they're fascinating.

### The Basal Polytomy: Architectural Completeness

The most striking feature is a large polytomy at the base of the tree—essentially a place where multiple branches emerge from the same point, indicating that these implementations are architecturally indistinguishable given our character set. This group includes:

- Weizenbaum's original MADSLIP version
- Cosell's 1966 LISP implementation  
- Several other LISP versions from the 1960s
- Anthony Hay's 2021 C++ version
- Implementations in SNOBOL, Algol, and Java

These programs span six decades and completely different programming paradigms, yet they all preserve the complete ELIZA architectural suite: rule-based pattern matching with keyword priority, synonym translation, backtracking, wildcard tokens, parse segment storage, template-based reconstruction, conversational memory with ordered recall, fallback rules, punctuation-sensitive tokenization, list-and-symbol processing, and separable script architecture.

The fact that Cosell's independent reconstruction from verbal description clusters with the original is particularly meaningful. It validates both the robustness of Weizenbaum's conceptual design and the power of informal knowledge transmission in early computing communities. Similarly, the fact that Anthony Hay's 2021 C++ version clusters with 1960s implementations confirms that careful historical reconstruction can successfully recover the original architecture.

### The Derived Clade: Simplification and Adaptation

Branching off from this architecturally complete core is a well-supported group showing substantial architectural divergence. This clade contains mostly BASIC implementations from the 1970s and 80s, along with various Python and JavaScript versions and some simplified LISP implementations.

These versions show patterns of feature loss driven by different constraints and contexts. The BASIC implementations that circulated through hobbyist magazines often lack backtracking pattern matchers and conversational memory—simplifications driven by the limited memory and processing power of early microcomputers, but also by pedagogical choices. These were programs meant to be typed in by amateur programmers and understood by beginners.

The Python implementations show different simplification patterns, sometimes preserving some features while abandoning others like keyword priority ranking or separable script architecture. These choices reflect how modern programming paradigms—object-oriented design, web frameworks—shaped how ELIZA's concepts got reimplemented.

Interestingly, Peter Norvig's LISP version from 1991 appears in this derived clade despite being written in LISP. It lacks several features present in the complete implementations: keyword priority, synonym translation, fallback rules, separable scripts. This was clearly a deliberate pedagogical choice—Norvig was illustrating core pattern-matching concepts in minimal, readable code, not trying to faithfully reproduce the complete ELIZA architecture.

### The Validation Tests: What They Tell Us

The clone test worked perfectly: the two byte-for-byte identical copies cluster together with zero branch length, exactly as they should.

The control specimen—my file system utility—revealed something important and somewhat problematic. It nests within the derived ELIZA clade rather than appearing as an outgroup. This tells us that the thirteen characters we're using capture general pattern-matching program architecture rather than ELIZA-specific conversational features. A file utility and a chatbot can share enough architectural features to cluster together when we only consider structural characters.

This finding points to a refinement needed in future work: we should include features more specific to conversational coherence and therapeutic dialogue patterns that would distinguish chatbots from other pattern-matching systems. But it also makes a deeper point: structural similarity alone doesn't constitute participation in the ELIZA tradition.

### The Extreme Outlier: Critical Engagement

LadyMouth scores zero on every single architectural character. No pattern matching, no keywords, no templates, no memory—nothing. Yet it explicitly engages with ELIZA through its name, its invocation of therapeutic dialogue, and its critical feminist examination of language, gender, and computational authority.

LadyMouth represents what I'd call critical participation in the ELIZA tradition—engagement through interrogation and rejection rather than preservation. Where canonical implementations accept the premise that therapeutic dialogue can be simulated through pattern matching, LadyMouth questions that premise by using fundamentally different mechanisms. Its maximal phylogenetic distance from all other implementations accurately represents its relationship: connected through discourse and positioning, distant through architectural choice.

---

## What This Actually Means: Software as Textual Tradition

Here's where things get philosophically interesting. The phylogenetic tree we've produced isn't really a phylogeny in the biological sense, nor is it a classical stemma like you'd find in textual criticism of ancient manuscripts. It's something else.

### The Problem with Rigid Designation

In biology, an orange is an orange because of material continuity—it grew on an orange tree that descended through an unbroken chain from ancestral citrus lineages. Philosopher Saul Kripke called this "rigid designation" in natural kinds: physical continuity provides the anchor for reference.

But software fundamentally lacks this anchor. You can recreate a program from scratch, guided only by a description, and end up with something functionally identical to the original despite sharing no material history. That's exactly what happened with Cosell's LISP version—Weizenbaum described the algorithm in conversation, and Cosell built it independently.

Moreover, programs get copied, modified, compiled, decompiled, translated across languages, and reconstituted from fragments. Each transformation can sever the "chain of custody" that would establish lineage in the biological sense.

So what counts as "an ELIZA"? If it's defined by specific architectural features, then many programs commonly called ELIZA aren't ELIZAs at all. If it's defined by the name and social positioning, then we're in the realm of convention rather than objective structure. And to make matters worse, many implementations conflate ELIZA (the language-processing framework) with DOCTOR (the specific psychiatric script), losing the platform/script separation that was central to Weizenbaum's original design.

### Reframing as Textual Tradition

Instead of treating ELIZA implementations as imperfect copies of an original, I've found it more useful to view each as a distinct version within a textual tradition. This framework comes from literary scholarship—particularly Jerome McGann's work on "bibliographical codes" and D.F. McKenzie's "sociology of texts."

The idea is that each implementation exists in its own social and material context. A 1960s LISP version with terse variable names reflects the constraints of its time—scarce memory, expensive computing, programmers working in isolation. A modern Python version with extensive docstrings and type hints reflects contemporary norms of collaborative development and software engineering pedagogy. These aren't superficial differences; they're constitutive features of each version's identity.

Under this view, authenticity can't be located in an "original" (which few people besides Weizenbaum ever saw anyway), but rather in the integrity of each version within its own context. Each implementation is part of a tradition, bearing relations to what came before and enabling what comes after.

### The Naming Paradox

This brings us to what I call the naming paradox. Self-identification through naming is actually the opposite of rigid designation. It represents intentional adoption into a tradition rather than material continuity.

Consider LadyMouth. It says "I am ELIZA" not because of descent but because of critical engagement with the ELIZA concept. My file utility shares architectural features with ELIZA but makes no such claim. In the textual tradition framework, both positions are meaningful data. When someone names their program ELIZA, they're performing an act of acknowledgment (recognizing the tradition exists), positioning (claiming a relationship to it), and interpretation (offering their understanding of what ELIZA means).

So the phylogenetic analysis doesn't become irrelevant when identity is socially constructed—rather, it reveals the structure of that social construction. The tree maps different modes of participation in a textual tradition: faithful preservation, adaptation through different contexts, convergent reimplementation, critical intervention through rejection.

---

## What We Learned from the Original Code

Finding and analyzing the original ELIZA code revealed some surprises that challenge the conventional narrative:

**It wasn't written in LISP.** Everyone thought ELIZA was a LISP program, but it was actually written in MAD-SLIP—a list-processing extension to Michigan Algorithm Decoder that Weizenbaum had developed. Interestingly, Weizenbaum's paper on SLIP doesn't mention John McCarthy's LISP even once, which seems like a deliberate snub in the competitive world of early AI research. There's even a line in the SLIP paper that reads something like "some people love their homebrew programming languages too much, whereas what we really need is to put this in Fortran, which everybody uses." The irony is that LISP ended up being how most people experienced ELIZA, making it a showcase for the very thing Weizenbaum was implicitly criticizing.

**ELIZA could learn.** The code contains a whole teaching component that's mentioned only briefly in Weizenbaum's paper and was completely lost to history. You could type a command (I think it was "learn" or something similar) and actually train ELIZA in real time by entering new rules in the S-expression format. This makes the naming make more sense—it's called ELIZA after Eliza Doolittle from Pygmalion, who learns proper speech. But since nobody had the code, nobody knew it could actually learn.

**It was a programmable framework.** Weizenbaum's student Paul Howard extended it so you could put MAD-SLIP code directly into the script. The script itself could contain conditional logic and modularized sub-routines. So ELIZA was more like a framework for writing sophisticated chatbots than just a single chatbot. The DOCTOR script was like a "Hello, World" example, not the entire system.

**The implementation details were sophisticated.** Anthony Hay discovered that it used a hash function and had these two critical SLIP functions called Match and Assemble that did the pattern matching and response assembly. These functions weren't in the published SLIP code—we had to go back to MIT's archives multiple times to track them down. The code went all the way down through MAD, SLIP, and something unfortunately named FAP (an API to call assembly routines from Fortran), all the way to 7090 machine code. Anthony actually got the whole thing running in modern C++.

---

## Where This Goes Next

This project is ongoing. We're still discovering things about the original code, still adding specimens to the corpus, still refining the character set to better capture what makes ELIZA-like programs distinctive from other pattern-matching systems.

A few directions I'm particularly interested in:

**Better conversational features**: The current character set needs features that specifically capture conversational coherence and dialogue patterns, not just pattern-matching architecture.

**Wider sampling**: There are probably hundreds more ELIZA implementations out there. I'd especially love to find more from the pre-microcomputer era, more from non-English-speaking countries, and more critical/artistic engagements like LadyMouth.

**Other software families**: The method we've developed should work for other software ecosystems with many descendants—adventure games, early operating systems, interpreters, AI toolkits. Any software family with a rich transmission history could be analyzed this way.

**Theoretical development**: I want to develop the theoretical framework further, thinking more carefully about how software traditions form, persist, and transform. How do naming practices work? What role do different types of documentation play? How do social networks of programmers shape what gets preserved and what gets lost?

---

## Why This Matters

You might wonder why anyone should care about reconstructing the genealogy of a 60-year-old chatbot. A few reasons:

**Understanding how ideas propagate**: Software is one of the few domains where we can actually watch ideas spread, mutate, and evolve in something close to real time (at least on historical timescales). The ELIZA genealogy shows how a concept can be transmitted through verbal description, formal publication, code copying, pedagogical simplification, and critical engagement—often simultaneously.

**Legal and practical implications**: Questions of who copied whose code matter for copyright, patents, and software licensing. The phylogenetic approach offers a principled way to think about relationships between programs that goes beyond simple clone detection.

**Historical methodology**: Much of computing history is told through narratives and personal recollections, which are valuable but incomplete and sometimes contradictory. A computational approach to software artifacts offers a complement to traditional historical methods.

**The nature of software itself**: What does it mean for one program to be "the same as" or "descended from" another? These aren't just academic questions—they go to the heart of what software is and how it differs from both biological organisms and cultural texts.

For me, though, the real fascination is in the discourse problem. ELIZA was never really about AI or natural language processing in the modern sense. It was about conversation, about the back-and-forth that happens when you try to understand something or someone. That's what science is about too—you're having a kind of discourse with the domain you're studying, asking questions and getting answers back.

We still haven't solved discourse computing, even after sixty years of work since ELIZA. LLMs are impressive, but they're not having a conversation in the sense that ELIZA tried to have one—they're predicting text, not maintaining context and intention across multiple turns. The key insight from ELIZA, the thing that made people think they were talking to something intelligent, was that it reflected what you said back to you in a transformed way. It maintained a kind of coherence through the conversation.

That's what we've lost track of, and maybe why genealogy matters here. By understanding how ELIZA spread and what got preserved and what got lost in each transmission, we might rediscover what was important about it in the first place.

---

## Acknowledgments

This project wouldn't exist without Team ELIZA: David Berry, Sarah Ciston, Anthony Hay, Rupert Lane, Mark Marino, Peter Millican, Art Schwarz, and Peggy Weil have provided years of invaluable discussion and technical help. Anthony Hay and Art Schwarz were particularly crucial for detailed technical analysis. Rupert Lane obtained the SNOBOL and ALGOL code. Myles Crowley at the MIT archives made it possible to access Weizenbaum's papers remotely during COVID. The Weizenbaum estate graciously granted permission to open-source the archival materials.

Some of the programming and writing for this project was aided by OpenAI and Anthropic LLMs, which is both ironic and appropriate given the subject matter.

All code, data, and source materials are available at https://github.com/jeffshrager/elizagen.org/tree/master/genealogy

---

*This is a work in progress. If you have an ELIZA implementation I should know about, or if you find errors or have suggestions, please reach out. The methodology is solid but the corpus is always growing, and I'm still learning what these genealogies can tell us about how software evolves.*