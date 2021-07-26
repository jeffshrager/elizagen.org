/*  This is a simulation of Joseph Weizenbaum's 1966 ELIZA. The code was
    written from scratch using only the description in the article on page
    36 of the January 1966 edition of Communications of the ACM as a guide.
    If given the same S-expression-like script from the appendix of that
    article, and given the same prompt text, the conversation it generates
    is identical to the conversation shown in that article.

    I made this for my amusement and hereby place it in the public domain
    or, if you prefer, I release it under either CC0 1.0 Universal or the
    MIT License.
    -- Anthony Hay, 2021, Devon, UK
*/


#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <chrono>
#include <ctime>
#include <cassert>
#include <vector>
#include <memory>
#include <map>
#include <chrono>
#include <algorithm>
#include <deque>
#include <cctype>
#include <thread>



namespace micro_test_library {

unsigned g_test_count;      // total number of tests executed
unsigned g_fault_count;     // total number of tests that fail


// write a message to std::cout if !(value == expected_value)
// (this is a function so that value and expected_value are evaluated only once)
template<typename A, typename B>
void test_equal(const A & value, const B & expected_value,
    const char * filename, const size_t linenum, const char * functionname)
{
    ++g_test_count;
    if (!(value == expected_value)) {
        ++g_fault_count;
        // e.g. love.cpp(2021) : in proposal() expected 'Yes!', but got 'Hahaha'
        std::cout
            << filename << '(' << linenum
            << ") : in " << functionname
            << "() expected '" << expected_value
            << "', but got '" << value
            << "'\n";
    }
}

#define TEST_EQUAL(value, expected_value)                   \
{                                                           \
    micro_test_library::test_equal(value, expected_value,    \
        __FILE__, __LINE__, __FUNCTION__);                  \
}

} //namespace mico_test_library







/*  In the code below there are occasional references to Joseph
    Weizenbaum's 1966 CACM article. A reference like [page X (Y)] refers
    to paragraph Y on page X of that publication.
*/

typedef std::deque<std::string> stringlist; // (stands in for Wiezenbaum's Slip)


// remove front element of given 'container' and return it
template<typename T>
auto pop_front(T & container)
{
    auto v(container.front());
    container.pop_front();
    return v;
}

template<>
auto pop_front(std::string & container)
{
    auto v(container.front());
    container.erase(0, 1);
    return v;
}


// join given 'words' into one space separated string; punctuation is attached
// to previous word e.g. join(["one", "two", ",", "3", "?"]) -> "one two, 3?"
std::string join(const stringlist & words)
{
    std::string result;
    for (const auto & word : words) {
        if (!word.empty()) {
            //if (!result.empty() && (std::isalnum(word[0]) || word[0] == '\''))
            if (!result.empty())
                result += ' ';
            result += word;
        }
    }
    return result;
}





//////// //       //// ////////    ///    //        ///////   //////   ////  //////  
//       //        //       //    // //   //       //     // //    //   //  //    // 
//       //        //      //    //   //  //       //     // //         //  //       
//////   //        //     //    //     // //       //     // //   ////  //  //       
//       //        //    //     ///////// //       //     // //    //   //  //       
//       //        //   //      //     // //       //     // //    //   //  //    // 
//////// //////// //// //////// //     // ////////  ///////   //////   ////  //////  


namespace elizalogic { // the core ELIZA algorithm

// map tag -> associated words, e.g. "BELIEF" -> ("BELIEVE" "FEEL" "THINK" "WISH")
typedef std::map<std::string, stringlist> tagmap;


bool punctuation(char c)
{
    return c == ',' || c == '.'; // [page 37 (c)]
}

bool punctuation(const std::string & s)
{
    return s.size() == 1 && punctuation(s[0]);
}


// split given string 's' into a list of "words"; punctuation are words
// e.g. split("one   two, three?") -> ["one", "two", ",", "three?"]
stringlist split(std::string s)
{
    stringlist result;
    std::string word;
    for (auto ch : s) {
        if (punctuation(ch) || ch == ' ') {
            if (!word.empty()) {
                result.push_back(word);
                word.clear();
            }
            if (ch != ' ')
                result.push_back(std::string(1, ch));
        }
        else
            word.push_back(ch);
    }
    if (!word.empty())
        result.push_back(word);
    return result;
}


// return given string 's' in uppercase
std::string to_upper(std::string s)
{
    std::transform(s.begin(), s.end(), s.begin(),
        [](unsigned char c) { return static_cast<char>(std::toupper(c)); }
    );
    return s;
}


// return numeric value of given 's' or -1, e.g. to_int("2") -> 2, to_int("two") -> -1
int to_int(const std::string & s)
{
    int result = 0;
    for (auto c : s) {
        if (std::isdigit(c))
            result = 10 * result + c - '0';
        else
            return -1;
    }
    return result;
}


// e.g. inlist("DEPRESSED", "(*SAD HAPPY DEPRESSED)") -> true
// e.g. inlist("FATHER", "(/FAMILY)") -> true (assuming tags("FAMILY") -> "... FATHER ...")
bool inlist(const std::string & word, std::string wordlist, const tagmap & tags)
{
    if (wordlist.back() == ')')
        wordlist.pop_back();
    const char * cp = wordlist.data();
    if (*cp == '(')
        ++cp;
    stringlist s;
    if (*cp == '*') {
        ++cp;
        while (*cp == ' ')
            ++cp;
        s = split(std::string(cp));
    }
    else if (*cp == '/') {
        ++cp;
        while (*cp == ' ')
            ++cp;
        const auto t = tags.find(std::string(cp));
        if (t != tags.end())
            s = t->second;
    }
    return std::find(s.begin(), s.end(), word) != s.end();
}


/*  return true iff 'words' match 'pattern'; if they match, 'matching_components'
    are the actual matched words, one for each element of 'pattern'

    e.g. match(tags, [0, YOU, (* WANT NEED), 0], [YOU, NEED, NICE, FOOD], mc) -> true
      with mc = [<empty>, YOU, NEED, NICE FOOD]

    Note that grouped words in pattern, such as (* WANT NEED), must be presented
    as a single stringlist entry.
*/
bool match(const tagmap & tags, stringlist pattern, stringlist words, stringlist & matching_components)
{
    matching_components.clear();
    if (pattern.empty())
        return words.empty();

    auto patword = pop_front(pattern);
    int n = to_int(patword);
    if (n < 0) { // patword is e.g. "ARE" or "(*SAD HAPPY DEPRESSED)"
        if (words.empty())
            return false; // patword cannot match nothing
        std::string current_word = pop_front(words);
        if (patword.front() == '(') {
            // patword is a group, is current_word in that group?
            if (!inlist(current_word, patword, tags))
                return false;
        }
        else if (patword != current_word)
            return false; // patword is a single word and it doesn't match

        // so far so good; can we match remainder of pattern with remainder of words?
        stringlist mc;
        if (match(tags, pattern, words, mc)) {
            matching_components.push_back(current_word);
            matching_components.insert(matching_components.end(), mc.begin(), mc.end());
            return true;
        }
    }
    else if (n == 0) { // 0 matches zero or more of any words
        stringlist component;
        stringlist mc;
        for (;;) {
            if (match(tags, pattern, words, mc)) {
                matching_components.push_back(join(component));
                matching_components.insert(matching_components.end(), mc.begin(), mc.end());
                return true;
            }

            if (words.empty())
                return false;

            component.push_back(pop_front(words));
        }
    }
    else { // match exactly n of any words [page 38 (a)]
        if (words.size() < static_cast<size_t>(n))
            return false;
        stringlist component;
        for (int i = 0; i < n; ++i)
            component.push_back(pop_front(words));
        stringlist mc;
        if (match(tags, pattern, words, mc)) {
            matching_components.push_back(join(component));
            matching_components.insert(matching_components.end(), mc.begin(), mc.end());
            return true;
        }
    }
    return false;
}


// return words constructed from given 'reassembly_rule' and 'components'
// e.g. reassemble([ARE, YOU, 1], [MAD, ABOUT YOU]) -> [ARE, YOU, MAD]
stringlist reassemble(const stringlist & reassembly_rule, const stringlist & components)
{
    stringlist result;
    for (const auto & r : reassembly_rule) {
        int n = to_int(r);
        if (n < 0)
            result.push_back(r);
        else if (n == 0 || static_cast<size_t>(n) > components.size())
            result.push_back("SCRIPT-ERROR-REASSEMBLY-RULE-INDEX-OUT-OF-RANGE");
        else {
            stringlist expanded = split(components[n - 1]);
            result.insert(result.end(), expanded.begin(), expanded.end());
        }
    }
    return result;
}





/*  This is my understanding of the ELIZA script file from the explanation
    in the CACM article and observation of the example script given in the
    APPENDIX to the CACM article:

    The script is a series of S-expressions. The first S-expression is a list
    of words that ELIZA will emit at startup [page 42 (a)]. This is followed
    by the atom START. Following that are the pattern-matching and message
    assembly Rules. Finally, an empty list.

    e.g.
        (HOW DO YOU DO.  PLEASE TELL ME YOUR PROBLEM)
        START
        <Rules>
        ()
    
    Each <Rule> has one of the following six forms:

    R1. Plain vanilla transformation rule. [page 38 (a)]
        (keyword [= keyword_substitution] [precedence]
            [ ((decomposition_pattern) (reassembly_rule) (reassembly_rule) ... )
               (decomposition_pattern) (reassembly_rule) (reassembly_rule) ... )
               :
               (decomposition_pattern) (reassembly_rule) (reassembly_rule) ... )) ] )
      e.g.
        (MY = YOUR 2
            ((0 YOUR 0 (/FAMILY) 0)
                (TELL ME MORE ABOUT YOUR FAMILY)
                (WHO ELSE IN YOUR FAMILY 5)
                (=WHAT)
                (WHAT ELSE COMES TO MIND WHEN YOU THINK OF YOUR 4))
            ((0 YOUR 0 (*SAD UNHAPPY DEPRESSED SICK ) 0)
                (CAN YOU EXPLAIN WHAT MADE YOU 5))
            ((0)
                (NEWKEY)))


    R2. Simple word substitution with no further transformation rules. [page 39 (a)]
        (keyword = keyword_substitution)
      e.g.
        (DONT = DON'T)
        (ME = YOU)


    R3. Allow words to be given tags, with optional word substitution. [page 41 (j)]
        (keyword [= keyword_substitution]
            DLIST (/ <word> ... <word>))
      e.g.
            (FEEL               DLIST(/BELIEF))
            (MOTHER             DLIST(/NOUN FAMILY))
            (MOM = MOTHER       DLIST(/ FAMILY))


    R4. Link to another keyword transformation rule. [page 40 (c)]
        (keyword [= keyword_substitution] [precedence]
            (= equivalence_class))
      e.g.
            (HOW                (=WHAT))
            (WERE = WAS         (=WAS))
            (DREAMED = DREAMT 4 (=DREAMT))
            (ALIKE 10           (=DIT))


    R5. As for R4 but allow pre-transformation before link. [page 40 (f)]
        (keyword [= keyword_substitution]
            ((decomposition_pattern)
                (PRE (reassembly_rule) (=equivalence_class))))
      e.g.
        (YOU'RE = I'M
            ((0 I'M 0)
                (PRE (I ARE 3) (=YOU))))


    R6. Rule to 'pre-record' responses for later use. [page 41 (f)]
        (MEMORY keyword
            (decomposition_pattern_1 = reassembly_rule_1)
            (decomposition_pattern_2 = reassembly_rule_2)
            (decomposition_pattern_3 = reassembly_rule_3)
            (decomposition_pattern_4 = reassembly_rule_4))
      e.g.
        (MEMORY MY
            (0 YOUR 0 = LETS DISCUSS FURTHER WHY YOUR 3)
            (0 YOUR 0 = EARLIER YOU SAID YOUR 3)
            (0 YOUR 0 = BUT YOUR 3)
            (0 YOUR 0 = DOES THAT HAVE ANYTHING TO DO WITH THE FACT THAT YOUR 3))


    In addition, there must be a NONE rule with the same form as R1. [page 41 (d)]
        (NONE
            ((0)
                (reassembly_rule)
                (reassembly_rule)
                :
                (reassembly_rule)) )
      e.g.
        (NONE
            ((0)
                (I AM NOT SURE I UNDERSTAND YOU FULLY)
                (PLEASE GO ON)
                (WHAT DOES THAT SUGGEST TO YOU)
                (DO YOU FEEL STRONGLY ABOUT DISCUSSING SUCH THINGS)))


    Each rule type may perform some sort of transformation on a
    suitable user input sentence. The ELIZA script is just a collection
    of these rules. Rules are generally triggered by the appearance of
    a particular keyword in the user input text. So one way to model
    the script is to std::map the keyword to the associated rule.

    In this implementation of ELIZA, the high-level core algorithm is
    encoded in the function eliza(), the transformation of the user's
    text into ELIZA's response is via polymorphic objects derived from
    rule_base, and the script is conveniently represented by a map of
    keywords to these rule_base objects.
 */


// interface and data shared by all rules
class rule_base {
public:
    rule_base() {}

    rule_base(const std::string & keyword, const std::string & word_substitution, int precedence)
        : keyword_(keyword), word_substitution_(word_substitution), precedence_(precedence)
    {}


    void add_transformation_rule(const stringlist & decomposition,
        const std::vector<stringlist> & reassembly_rules)
    {
        trans_.push_back(transform(decomposition, reassembly_rules));
    }

    int precedence() const { return precedence_; }
    std::string keyword() const { return keyword_; }


    enum action {
        inapplicable,   // no transformation could be performed
        complete,       // transformation of input is complete
        newkey,         // request caller try next keyword in keystack
        linkkey         // request caller try returned keyword
    };

    // replace 'word' with substitute specified by script rule, if any
    virtual action apply_word_substitution(std::string & word)
    {
        if (word_substitution_.empty() || word != keyword_)
            return inapplicable;
        word = word_substitution_;
        return complete;
    }

    // return true iff this rule has whole-sentence transformation rules associated with it
    virtual bool has_transformation() const { return false; }

    // use this rule's decomposition/reassembly rules to transform given 'words'
    virtual action apply_transformation(stringlist & /*words*/,
        const tagmap & /*tags*/, std::string & /*link_keyword*/)
    {
        return inapplicable;
    }

    virtual stringlist dlist_tags() const { return stringlist(); }

    virtual std::string to_string() const = 0;

protected:
    std::string keyword_;           // the word that triggers this rule
    std::string word_substitution_; // the word that is to replace the keyword, if any
    int precedence_{ 0 };           // the highest priority rule is selected first

    struct transform {              // decomposition and associated reassembly rules
        stringlist decomposition;
        std::vector<stringlist> reassembly_rules;
        unsigned next_reassembly_rule{ 0 };
        transform() {}
        transform(const stringlist & decomposition,
            const std::vector<stringlist> & reassembly_rules)
            : decomposition(decomposition), reassembly_rules(reassembly_rules)
        {}
    };
    std::vector<transform> trans_;  // transformations associated with this rule
};

std::string rule_base::to_string() const
{
    return std::string();
}


// map keyword -> transformation rule
typedef std::map<std::string, std::shared_ptr<rule_base>> rulemap;

// in the rules map, the special-cases NONE and MEMORY have keys
// that cannot match any user input text
#define SPECIAL_RULE_NONE   "zNONE"
#define SPECIAL_RULE_MEMORY "zMEMORY"



// e.g. (ME = YOU)
class rule_unconditional_substitution : public rule_base {
public:
    rule_unconditional_substitution() {}

    rule_unconditional_substitution(const std::string & keyword, const std::string & word_substitution)
        : rule_base(keyword, word_substitution, 0)
    {}

    virtual std::string to_string() const
    {
        return "(" + keyword_ + " = " + word_substitution_ + ")\n";
    }
};


// e.g. (MOM = MOTHER DLIST(/ FAMILY))
class rule_dlist : public rule_base {
public:
    rule_dlist() {}

    rule_dlist(const std::string & keyword, const std::string & word_substitution, const stringlist & tags)
        : rule_base(keyword, word_substitution, 0), tags_(tags)
    {
    }

    stringlist dlist_tags() const { return tags_; }

    virtual std::string to_string() const
    {
        std::string sexp("(");
        sexp += keyword_;
        if (!word_substitution_.empty())
            sexp += " = " + word_substitution_;
        sexp += " DLIST(" + join(tags_) + "))\n";
        return sexp;
    }

private:
    stringlist tags_;
};


// e.g. (DREAMED = DREAMT 4 (=DREAMT))
class rule_equivalence_class : public rule_base {
public:
    rule_equivalence_class() {}

    rule_equivalence_class(const std::string & keyword, const std::string & word_substitution,
        int precedence, const std::string & link_keyword)
        : rule_base(keyword, word_substitution, precedence), link_keyword_(link_keyword)
    {}

    virtual bool has_transformation() const { return true; }

    virtual action apply_transformation(stringlist & /*words*/, const tagmap & /*tags*/, std::string & link_keyword)
    {
        link_keyword = link_keyword_;
        return linkkey;
    }

    virtual std::string to_string() const
    {
        std::string sexp("(");
        sexp += keyword_;
        if (!word_substitution_.empty())
            sexp += " = " + word_substitution_;
        if (precedence_ > 0)
            sexp += " " + std::to_string(precedence_);
        sexp += " (= " + link_keyword_ + "))\n";
        return sexp;
    }

private:
    std::string link_keyword_;
};


/* e.g.
    (YOU'RE = I'M
        ((0 I'M 0)
            (PRE (I ARE 3) (=YOU))))
*/
class rule_pre : public rule_base {
public:
    rule_pre() {}

    rule_pre(std::string keyword, int precedence, std::string word_substitution,
        stringlist decomposition, stringlist reassembly, std::string link_keyword)
        : rule_base(keyword, word_substitution, precedence), link_keyword_(link_keyword)
    {
        std::vector<stringlist> reassembly_rules;
        reassembly_rules.push_back(reassembly);
        add_transformation_rule(decomposition, reassembly_rules);
    }

    virtual bool has_transformation() const { return true; }

    virtual action apply_transformation(stringlist & words, const tagmap & tags, std::string & link_keyword)
    {
        if (trans_.size() != 1)
            return inapplicable; // bad script?

        stringlist constituents;
        if (!match(tags, trans_[0].decomposition, words, constituents))
            return inapplicable; // [page 39 (f)] should not happen?

        if (trans_[0].reassembly_rules.size() != 1)
            return inapplicable; // bad script?
        stringlist & reassembly_rule = trans_[0].reassembly_rules[0];

        // use the selected reassembly rule and decomposition components
        // to construct a response sentence
        words = reassemble(reassembly_rule, constituents);

        link_keyword = link_keyword_;
        return linkkey;
    }

    virtual std::string to_string() const
    {
        std::string sexp("(");
        sexp += keyword_;
        if (!word_substitution_.empty())
            sexp += " = " + word_substitution_;
        sexp += "\n    ((" + join(trans_[0].decomposition) + ")";
        sexp += "\n        (PRE (" + join(trans_[0].reassembly_rules[0]) + ") (=" + link_keyword_ + "))))\n";
        return sexp;
    }

private:
    std::string link_keyword_;
};


/* e.g.
    (MEMORY MY
        (0 YOUR 0 = LETS DISCUSS FURTHER WHY YOUR 3)
        (0 YOUR 0 = EARLIER YOU SAID YOUR 3)
        (0 YOUR 0 = BUT YOUR 3)
        (0 YOUR 0 = DOES THAT HAVE ANYTHING TO DO WITH THE FACT THAT YOUR 3))
*/
class rule_memory : public rule_base {
public:
    rule_memory() {}

    rule_memory(const std::string & keyword)
        : rule_base(keyword, "", 0)
    {}

    void create_memory(const std::string & keyword, const stringlist & words, const tagmap & tags)
    {
        if (keyword != keyword_)
            return;

        assert(trans_.size() == num_transformations);
        const auto & transformation = trans_[memory_rule_index_];
        memory_rule_index_ = rand() % num_transformations; // rules are selected at random [page 41 (f)]

        stringlist constituents;
        if (!match(tags, transformation.decomposition, words, constituents))
            return;

        memories_.push_back(join(reassemble(transformation.reassembly_rules[0], constituents)));
    }

    bool is_time_for_recollection()
    {
        // is "a certain counting mechanism in a particular state?" [page 41 (f)]
        // with nothing further to go on, guess; use MEMORY every third call
        if (!memories_.empty())
            return a_certain_counting_mechanism_++ % 3 == 0;
        return false;
    }

    std::string recall_memory()
    {
        return memories_.empty() ? "" : pop_front(memories_);
    }

    virtual std::string to_string() const
    {
        std::string sexp("(MEMORY ");
        sexp += keyword_;
        for (const auto & k : trans_) {
            sexp += "\n    (" + join(k.decomposition);
            sexp += " = " + join(k.reassembly_rules[0]) + ")";
        }
        sexp += ")\n";
        return sexp;
    }

    // the MEMORY rule must have this number of transformations
    static constexpr int num_transformations = 4;

private:
    int memory_rule_index_{ num_transformations - 1 };
        // the CACM article says rules are selected at random [page 41 (f)],
        // but we'll make sure the first time we select a rule we select
        // the last rule so that this simulations gives the same response
        // to the "Bullies" input as the original ELIZA gave
    stringlist memories_;
    int a_certain_counting_mechanism_{ 0 };
 };


/* e.g.
    (MY = YOUR 2
        ((0 YOUR 0 (/FAMILY) 0)
            (TELL ME MORE ABOUT YOUR FAMILY)
            (WHO ELSE IN YOUR FAMILY 5)
            (=WHAT)
            (WHAT ELSE COMES TO MIND WHEN YOU THINK OF YOUR 4))
        ((0 YOUR 0 (*SAD UNHAPPY DEPRESSED SICK ) 0)
            (CAN YOU EXPLAIN WHAT MADE YOU 5))
            ((0)
            (NEWKEY)))
*/
class rule_vanilla : public rule_base {
public:
    rule_vanilla() {}

    rule_vanilla(const std::string & keyword, const std::string & word_substitution, int precedence)
        : rule_base(keyword, word_substitution, precedence)
    {}

    virtual bool has_transformation() const { return true; }

    virtual action apply_transformation(stringlist & words, const tagmap & tags, std::string & link_keyword)
    {
        if (trans_.empty())
            return inapplicable; // bad script?

        stringlist constituents;
        auto rule = trans_.begin();
        while (rule != trans_.end() && !match(tags, rule->decomposition, words, constituents))
            ++rule;
        if (rule == trans_.end())
            return inapplicable; // [page 39 (f)] should not happen?

        // get the next reassembly rule to be used for this decomposition rule
        stringlist & reassembly_rule = rule->reassembly_rules[rule->next_reassembly_rule];

        // update the reassembly rule index so that they all get cycled through
        rule->next_reassembly_rule++;
        if (rule->next_reassembly_rule == rule->reassembly_rules.size())
            rule->next_reassembly_rule = 0;

        // is it the special-case reassembly rule (NEWKEY)?
        if (reassembly_rule.size() == 1 && reassembly_rule[0] == "NEWKEY")
            return newkey; // yes, try the next highest priority keyword, if any

        // is it the special-case reassembly rule (=XXXX)?
        if (reassembly_rule.size() == 1 && reassembly_rule[0].size() > 1 && reassembly_rule[0][0] == '=') {
            link_keyword = reassembly_rule[0];
            pop_front(link_keyword); // pop off the '='
            return linkkey; // yes, try the next highest priority keyword, if any
        }

        // use the selected reassembly rule and decomposition components
        // to construct a response sentence
        words = reassemble(reassembly_rule, constituents);
        return complete;
    }

    virtual std::string to_string() const
    {
        std::string sexp("(");
        sexp += (keyword_ == SPECIAL_RULE_NONE) ? "NONE" : keyword_;
        if (!word_substitution_.empty())
            sexp += " = " + word_substitution_;
        if (precedence_ > 0)
            sexp += " " + std::to_string(precedence_);
        for (const auto & k : trans_) {
            sexp += "\n    ((" + join(k.decomposition) + ")";
            for (const auto & r : k.reassembly_rules)
                sexp += "\n        (" + join(r) + ")";
            sexp += ")";
        }
        sexp += ")\n";
        return sexp;
    }
};



// collect all tags from any of the given 'rules' that have them into a tagmap
tagmap collect_tags(const rulemap & rules)
{
    tagmap tags;
    for (auto & r : rules) {
        stringlist keyword_tags(r.second->dlist_tags());
        for (auto t : keyword_tags) {
            if (t == "/")
                continue;
            if (t.size() > 1 && t.front() == '/')
                pop_front(t);
            tags[t].push_back(r.second->keyword());
        }
    }
    return tags;
}


template<typename T>
auto get_rule(rulemap & rules, const std::string & keyword)
{
    auto rule = rules.find(keyword);
    if (rule == rules.end()) {
        std::string msg("SCRIPT ERROR FOR ");
        msg += keyword;
        throw std::runtime_error(msg);
    }
    auto castrule = std::dynamic_pointer_cast<T>(rule->second);
    if (!castrule) {
        std::string msg("INTERNAL ERROR FOR ");
        msg += keyword;
        throw std::runtime_error(msg);
    }
    return castrule;
}




// produce a response to the given 'input' using the given 'rules'
std::string eliza(rulemap & rules, const std::string & input)
{
    // for simplicity, convert the given input string to a list of uppercase words
    // e.g. "Hello, world!" -> (HELLO , WORLD !)
    stringlist words(split(to_upper(input)));


    // scan for keywords [page 38 (c)]; build the keystack; apply word substitutions
    stringlist keystack;
    int top_rank = 0;
    for (auto word = words.begin(); word != words.end(); ) {
        if (punctuation(*word)) {
            // keep only the first clause to contain a keyword [page 37 (c)]
            if (keystack.empty()) {
                // discard left of punctuation, continue scanning what remains
                word = words.erase(words.begin(), ++word);
                continue;
            }
            else {
                // discard right of punctuation, scan is complete
                word = words.erase(word, words.end());
                break;
            }
        }

        const auto r = rules.find(*word);
        if (r != rules.end()) {
            const auto & rule = r->second;
            if (rule->has_transformation()) {
                if (rule->precedence() > top_rank) {
                    // *word is a keyword with precedence higher than the highest
                    // keyword found previously: it goes top of the keystack [page 39 (d)]
                    keystack.push_front(*word);
                    top_rank = rule->precedence();
                }
                else {
                    // *word is a keyword with precedence lower than the highest
                    // keyword found previously: it goes bottom of the keystack
                    keystack.push_back(*word);
                }
            }
            rule->apply_word_substitution(*word); // [page 39 (a)]
        }

        ++word;
    }

    auto memory_rule = get_rule<rule_memory>(rules, SPECIAL_RULE_MEMORY);
    if (keystack.empty()) {
        // a text without keywords; can we recall a MEMORY? [page 41 (f)]
        if (memory_rule->is_time_for_recollection())
            return memory_rule->recall_memory();
    }

    // build tag mapping so that e.g. tags[BELIEF] -> (BELIEVE FEEL THINK WISH)
    const tagmap tags(collect_tags(rules));

    // the keystack contains all keywords that occur in the given 'input';
    // apply transformation associated with the top keyword [page 39 (d)]
    while (!keystack.empty()) {
        const std::string top_keyword = pop_front(keystack);

        auto rule = rules.find(top_keyword);
        if (rule == rules.end())
            break; // e.g. could happen if a rule links to a non-existent keyword

        // try to lay down a memory for future use
        memory_rule->create_memory(top_keyword, words, tags);

        // perform the transformation for this rule
        std::string link_keyword;
        auto act = rule->second->apply_transformation(words, tags, link_keyword);

        if (act == rule_base::complete)
            return join(words); // decomposition/reassembly successfully applied

        if (act == rule_base::inapplicable)
            break; // no decomposition rule matched the input words

        if (act == rule_base::linkkey)
            keystack.push_front(link_keyword); // rule links to another; loop

        // (rule_base::newkey -> rule wants to try next highest keyword, if any)
        assert(act == rule_base::linkkey || act == rule_base::newkey);
    }


    // last resort: the NONE rule never fails to produce a response [page 41 (d)]
    auto none_rule = get_rule<rule_vanilla>(rules, SPECIAL_RULE_NONE);
    std::string discard;
    none_rule->apply_transformation(words, tags, discard);
    return join(words);
}

}//namespace elizalogic





 //////// //       //// ////////    ///     //////   //////  ////////  //// ////////  //////// 
 //       //        //       //    // //   //    // //    // //     //  //  //     //    //    
 //       //        //      //    //   //  //       //       //     //  //  //     //    //    
 //////   //        //     //    //     //  //////  //       ////////   //  ////////     //    
 //       //        //    //     /////////       // //       //   //    //  //           //    
 //       //        //   //      //     // //    // //    // //    //   //  //           //    
 //////// //////// //// //////// //     //  //////   //////  //     // //// //           //    


namespace elizascript { // reader for 1966 ELIZA script file format


struct script {
    // ELIZA's opening remarks e.g. "HOW DO YOU DO.  PLEASE TELL ME YOUR PROBLEM"
    stringlist hello_message;

    // maps keywords -> transformation rules
    elizalogic::rulemap rules;
};



struct token {
    // types of token in an ELIZA script file
    enum class typ { eof, symbol, number, open_bracket, close_bracket };

    typ t{ typ::eof };
    std::string value;

    token() : t(typ::eof) {}
    token(typ t) : t(t) {}
    token(typ t, const std::string & value) : t(t), value(value) {}

    bool symbol()               const { return t == typ::symbol; }
    bool symbol(const char * v) const { return t == typ::symbol && value == v; }
    bool number()               const { return t == typ::number; }
    bool open()                 const { return t == typ::open_bracket; }
    bool close()                const { return t == typ::close_bracket; }
    bool eof()                  const { return t == typ::eof; }

    bool operator==(const token & rhs) const { return t == rhs.t && value == rhs.value; }
};



// this is just good enough to divide the ELIZA script file format
// into tokens useful to eliza_script_reader
template<typename T>
class tokenizer {
public:
    tokenizer(T & script_file) : stream_(script_file) {}

    // return the current token, but don't advance past it
    token peektok()
    {
        if (got_token_)
            return t_;
        got_token_ = true;
        return t_ = readtok();
    }

    // return the current token, advance read point past it
    token nexttok()
    {
        if (got_token_) {
            got_token_ = false;
            return t_;
        }
        return readtok();
    }

private:
    T & stream_;
    token t_;
    bool got_token_{ false };
    const static size_t buflen_ = 512;
    uint8_t buf_[buflen_];
    size_t bufdata_{ 0 };
    size_t bufptr_{ 0 };

    token readtok()
    {
        uint8_t ch;
        for (;;) {
            do { // skip whitespace
                if (!nextch(ch))
                    return (token(token::typ::eof));
            } while (is_whitespace(ch));
            if (ch != ';')
                break;
            do { // skip comment
                if (!nextch(ch))
                    return (token(token::typ::eof));
            } while (!is_newline(ch));
        }

        if (ch == '(')
            return (token(token::typ::open_bracket));

        if (ch == ')')
            return (token(token::typ::close_bracket));

        if (is_digit(ch)) {
            token t(token::typ::number);
            t.value.push_back(ch);
            while (peekch(ch) && is_digit(ch)) {
                t.value.push_back(ch);
                nextch(ch);
            }
            return t;
        }

        // anything else is a symbol
        token t(token::typ::symbol);
        t.value.push_back(ch);
        while (peekch(ch) && !non_symbol(ch)) {
            t.value.push_back(ch);
            nextch(ch);
        }
        return t;
    }

    bool nextch(uint8_t & ch)
    {
        if (peekch(ch)) {
            ++bufptr_;
            return true;
        }
        return false;
    }

    bool peekch(uint8_t & ch)
    {
        if (bufptr_ == bufdata_)
            refilbuf();
        if (bufptr_ == bufdata_)
            return false;
        ch = buf_[bufptr_];
        return true;
    }
    
    void refilbuf()
    {
        bufptr_ = bufdata_ = 0;
        if (!stream_.eof()) {
            stream_.read(reinterpret_cast<char *>(buf_), buflen_);
            bufdata_ = static_cast<size_t>(stream_.gcount());
        }
    }

    inline bool is_whitespace(uint8_t ch)
    {
        return ch <= 0x20 || ch == 0x7F;
        // this must hold: is_newline(ch) => is_whitespace(ch)
    }

    inline bool is_newline(uint8_t ch)
    {
        return ch == '\x0A'     // LF
            || ch == '\x0B'     // VT
            || ch == '\x0C'     // FF
            || ch == '\x0D';    // CR
    }

    inline bool is_digit(uint8_t ch)
    {
        return unsigned(ch) - '0' < 10;
    }

    inline bool non_symbol(uint8_t ch)
    {
        return ch == '(' || ch == ')' || ch == ';' || is_whitespace(ch);
    }
};





template<typename T>
class eliza_script_reader {
public:

    eliza_script_reader(T & script_file, script & s)
        : tok_(script_file), script_(s)
    {
        script_.hello_message = rdlist();
        if (tok_.peektok().symbol("START"))
            tok_.nexttok(); // skip over START, if present

        while (read_rule())
            ;
    }

private:
    tokenizer<T> tok_;
    script & script_;


    // in the following comments, @ = position in symbol stream on function entry

    // return words between opening and closing brackets
    // if prior is true nexttok() should be the opening bracket, e.g. @(WORD WORD 0 WORD)
    // if prior is false nexttok() should be the first symbol following the
    // opening bracket, e.g. (@WORD WORD 0 WORD)
    stringlist rdlist(bool prior = true)
    {
        stringlist s;

        token t = tok_.nexttok();
        if (prior) {
            if (!t.open())
                throw std::runtime_error("expected '('");
            t = tok_.nexttok();
        }
        while (!t.close()) {
            if (t.t == token::typ::symbol)
                s.emplace_back(t.value);
            else if (t.number())
                s.emplace_back(t.value);
            else if (t.open()) {
                // embed entire sublist in one sub-string
                std::string sublist;
                t = tok_.nexttok();
                while (!t.close()) {
                    if (!t.symbol())
                        throw std::runtime_error("expected symbol");
                    if (!sublist.empty())
                        sublist += ' ';
                    sublist += t.value;
                    t = tok_.nexttok();
                }
                s.emplace_back("(" + sublist + ")");
            }
            else
                throw std::runtime_error("expected ')'");
            t = tok_.nexttok();
        }

        return s;
    }


    /* e.g.
        (YOU'RE = I'M
            ((0 I'M 0)
                (@PRE (I ARE 3) (=YOU))))
    */
    bool read_pre_rule(const std::string & keyword,
        int precedence,
        const std::string & keyword_substitution,
        const stringlist & decomposition)
    {
        tok_.nexttok(); // skip PRE
        stringlist reassembly = rdlist();

        if (!tok_.nexttok().open())
            throw std::runtime_error("expected '('");
        token t = tok_.nexttok();
        std::string link_keyword;
        if (t.symbol("=")) {
            t = tok_.nexttok();
            if (!t.symbol())
                throw std::runtime_error("expected equivalence class name");
            link_keyword = t.value;
        }
        else if (t.symbol() && t.value.size() > 1 && t.value[0] == '=')
            link_keyword = std::string(reinterpret_cast<const char *>(&t.value[1]), t.value.size() - 1);
        else
            throw std::runtime_error("expected equivalence class name");

        if (!tok_.nexttok().close())
            throw std::runtime_error("expected ')'");
        if (!tok_.nexttok().close())
            throw std::runtime_error("expected ')'");
        if (!tok_.nexttok().close())
            throw std::runtime_error("expected ')'");
        if (!tok_.nexttok().close())
            throw std::runtime_error("expected ')'");

        script_.rules[keyword] = std::make_shared<elizalogic::rule_pre>(keyword, precedence,
            keyword_substitution, decomposition, reassembly, link_keyword);

        return true;
    }


    /* e.g.
        (MY = YOUR 2
            (@(0 YOUR 0 (/FAMILY) 0)
                (TELL ME MORE ABOUT YOUR FAMILY)
                (WHO ELSE IN YOUR FAMILY 5)
                (YOUR 4)
                (WHAT ELSE COMES TO MIND WHEN YOU THINK OF YOUR 4))
            ((0 YOUR 0 (*SAD UNHAPPY DEPRESSED SICK ) 0)
                (CAN YOU EXPLAIN WHAT MADE YOU 5))
            ((0)
                (NEWKEY)))
    */
    bool read_vanilla_rule(const std::string & keyword,
        const std::string & keyword_substitution, int precedence) // or PRE
    {
        auto r = std::make_shared<elizalogic::rule_vanilla>(
            keyword, keyword_substitution, precedence);

        for (;;) {
            const stringlist decomposition = rdlist();
            std::vector<stringlist> reassembly_rules;

            token t = tok_.nexttok();
            for (;;) {
                if (!t.open())
                    throw std::runtime_error("expected '('");
                if (tok_.peektok().symbol("PRE"))
                    return read_pre_rule(keyword, precedence, keyword_substitution, decomposition);
                reassembly_rules.push_back(rdlist(false));
                t = tok_.nexttok();
                if (t.close())
                    break;
            }
            r->add_transformation_rule(decomposition, reassembly_rules);

            t = tok_.nexttok();
            if (t.close())
                break;
            if (!t.open())
                throw std::runtime_error("expected '('");
        }
        script_.rules[keyword] = r;

        return true;
    }


    /* e.g.
        (NONE@
            ((0)
                (I AM NOT SURE I UNDERSTAND YOU FULLY)
                (PLEASE GO ON)
                (WHAT DOES THAT SUGGEST TO YOU)
                (DO YOU FEEL STRONGLY ABOUT DISCUSSING SUCH THINGS)))
    */
    bool read_none()
    {
        tok_.nexttok();
        return read_vanilla_rule(SPECIAL_RULE_NONE, "", 0);
    }


    /* e.g.
        (MEMORY@ MY
            (0 YOUR 0 = LETS DISCUSS FURTHER WHY YOUR 3)
            (0 YOUR 0 = EARLIER YOU SAID YOUR 3)
            (0 YOUR 0 = BUT YOUR 3)
            (0 YOUR 0 = DOES THAT HAVE ANYTHING TO DO WITH THE FACT THAT YOUR 3))
    */
    bool read_memory()
    {
        token t = tok_.nexttok();
        if (!t.symbol())
            throw std::runtime_error("expected keyword");
        auto r = std::make_shared<elizalogic::rule_memory>(t.value);

        for (int i = 0; i < elizalogic::rule_memory::num_transformations; ++i) {
            stringlist decomposition;
            std::vector<stringlist> reassembly_rules;

            if (!tok_.nexttok().open())
                throw std::runtime_error("expected '('");
            for (t = tok_.nexttok(); !t.symbol("=") && !t.eof(); t = tok_.nexttok())
                decomposition.push_back(t.value);
            stringlist reassembly;
            for (t = tok_.nexttok(); !t.close() && !t.eof(); t = tok_.nexttok())
                reassembly.push_back(t.value);
            reassembly_rules.push_back(reassembly);

            r->add_transformation_rule(decomposition, reassembly_rules);
        }

        if (!tok_.nexttok().close())
            throw std::runtime_error("expected ')'");

        script_.rules[SPECIAL_RULE_MEMORY] = r;
        return true;
    }


    // e.g. (MOM = MOTHER DLIST@(/ FAMILY))
    bool dlist(const std::string & keyword,
        const std::string & keyword_substitution)
    {
        stringlist tags;
        tags = rdlist();
        token t = tok_.nexttok();
        if (!t.close())
            throw std::runtime_error("expected ')'");
        script_.rules[keyword] = std::make_shared<elizalogic::rule_dlist>(keyword, keyword_substitution, tags);
        return true;
    }


    // e.g. (DREAMED = DREAMT 4 (@=DREAMT))
    bool read_equivalence_class(const std::string & keyword,
        const std::string & keyword_substitution, int precedence)
    {
        std::string class_name;
        token t = tok_.nexttok();
        if (t.symbol("=")) {
            t = tok_.nexttok();
            if (!t.symbol())
                throw std::runtime_error("expected equivalence class name");
            class_name = t.value;
        }
        else if (t.symbol() && t.value.size() > 1 && t.value[0] == '=')
            class_name = std::string(reinterpret_cast<const char *>(&t.value[1]), t.value.size() - 1);
        else
            throw std::runtime_error("expected equivalence class name");

        script_.rules[keyword] = std::make_shared<elizalogic::rule_equivalence_class>(
            keyword, keyword_substitution, precedence, class_name);

        if (!tok_.nexttok().close())
            throw std::runtime_error("expected ')'");
        if (!tok_.nexttok().close())
            throw std::runtime_error("expected ')'");

        return true;
    }


    // read one rule of any type; return false => end of file reached
    bool read_rule()
    {
        std::string keyword, keyword_substitution;
        int precedence = 0;

        token t = tok_.nexttok();
        if (t.t == token::typ::eof)
            return false;
        if (!t.open())
            throw std::runtime_error("expected '('");

        t = tok_.nexttok();
        if (t.t == token::typ::close_bracket)
            return true; // ignore empty rule list, if present
        if (t.t != token::typ::symbol)
            throw std::runtime_error("expected keyword|MEMORY|NONE");
        if (t.value == "MEMORY")
            return read_memory();
        else if (t.value == "NONE")
            return read_none();
        keyword = t.value;

        for (;;) {
            t = tok_.nexttok();
            if (t.symbol("=")) {
                t = tok_.nexttok();
                if (t.t != token::typ::symbol)
                    throw std::runtime_error("expected keyword");
                keyword_substitution = t.value;
            }
            else if (t.number())
                precedence = std::stoi(t.value);
            else if (t.symbol("DLIST"))
                return dlist(keyword, keyword_substitution);
            else if (t.open()) {
                t = tok_.peektok();
                if (t.symbol() && t.value[0] == '=')
                    return read_equivalence_class(keyword, keyword_substitution, precedence);
                return read_vanilla_rule(keyword, keyword_substitution, precedence);
            }
            else if (t.t == token::typ::close_bracket) {
                // e.g. (ME = YOU)
                script_.rules[keyword] = std::make_shared<elizalogic::rule_unconditional_substitution>(
                    keyword, keyword_substitution);
                return true;
            }
            else
                throw std::runtime_error("malformed rule");
        }

        return true;
    }
};


template<typename T>
void read(T & script_file, script & s)
{
    eliza_script_reader<T> reader(script_file, s);
}


}//namespace elizascript






 //////// //       //// ////////    ///    //////// ////////  //////  //////// 
 //       //        //       //    // //      //    //       //    //    //    
 //       //        //      //    //   //     //    //       //          //    
 //////   //        //     //    //     //    //    //////    //////     //    
 //       //        //    //     /////////    //    //             //    //    
 //       //        //   //      //     //    //    //       //    //    //    
 //////// //////// //// //////// //     //    //    ////////  //////     //    


namespace elizatest { // basic test of whether this simulation is accurate

// return a string in ELIZA script format representing given script 's'
std::string to_string(const elizascript::script & s)
{
    std::string result;
    result += "(" + join(s.hello_message) + ")\n";
    result += "START\n";
    for (const auto r : s.rules)
        result += r.second->to_string();
    result += "()\n";
    return result;
}

// perform basic checks on implementation
void test()
{
    // script_text is logically identical to the script in the CACM article
    // appendix, but the ordering and whitespace is different so that it can
    // be checked against the output of elizatest::to_string()
    const char * script_text =
        "(HOW DO YOU DO. PLEASE TELL ME YOUR PROBLEM)\n"
        "START\n"
        "(ALIKE 10 (= DIT))\n"
        "(ALWAYS 1\n"
        "    ((0)\n"
        "        (CAN YOU THINK OF A SPECIFIC EXAMPLE)\n"
        "        (WHEN)\n"
        "        (WHAT INCIDENT ARE YOU THINKING OF)\n"
        "        (REALLY, ALWAYS)))\n"
        "(AM = ARE\n"
        "    ((0 ARE YOU 0)\n"
        "        (DO YOU BELIEVE YOU ARE 4)\n"
        "        (WOULD YOU WANT TO BE 4)\n"
        "        (YOU WISH I WOULD TELL YOU YOU ARE 4)\n"
        "        (WHAT WOULD IT MEAN IF YOU WERE 4)\n"
        "        (=WHAT))\n"
        "    ((0)\n"
        "        (WHY DO YOU SAY 'AM')\n"
        "        (I DON'T UNDERSTAND THAT)))\n"
        "(ARE\n"
        "    ((0 ARE I 0)\n"
        "        (WHY ARE YOU INTERESTED IN WHETHER I AM 4 OR NOT)\n"
        "        (WOULD YOU PREFER IF I WEREN'T 4)\n"
        "        (PERHAPS I AM 4 IN YOUR FANTASIES)\n"
        "        (DO YOU SOMETIMES THINK I AM 4)\n"
        "        (=WHAT))\n"
        "    ((0 ARE 0)\n"
        "        (DID YOU THINK THEY MIGHT NOT BE 3)\n"
        "        (WOULD YOU LIKE IT IF THEY WERE NOT 3)\n"
        "        (WHAT IF THEY WERE NOT 3)\n"
        "        (POSSIBLY THEY ARE 3)))\n"
        "(BECAUSE\n"
        "    ((0)\n"
        "        (IS THAT THE REAL REASON)\n"
        "        (DON'T ANY OTHER REASONS COME TO MIND)\n"
        "        (DOES THAT REASON SEEM TO EXPLAIN ANYTHING ELSE)\n"
        "        (WHAT OTHER REASONS MIGHT THERE BE)))\n"
        "(BELIEVE DLIST(/BELIEF))\n"
        "(BROTHER DLIST(/FAMILY))\n"
        "(CAN\n"
        "    ((0 CAN I 0)\n"
        "        (YOU BELIEVE I CAN 4 DON'T YOU)\n"
        "        (=WHAT)\n"
        "        (YOU WANT ME TO BE ABLE TO 4)\n"
        "        (PERHAPS YOU WOULD LIKE TO BE ABLE TO 4 YOURSELF))\n"
        "    ((0 CAN YOU 0)\n"
        "        (WHETHER OR NOT YOU CAN 4 DEPENDS ON YOU MORE THAN ON ME)\n"
        "        (DO YOU WANT TO BE ABLE TO 4)\n"
        "        (PERHAPS YOU DON'T WANT TO 4)\n"
        "        (=WHAT)))\n"
        "(CANT = CAN'T)\n"
        "(CERTAINLY (= YES))\n"
        "(CHILDREN DLIST(/FAMILY))\n"
        "(COMPUTER 50\n"
        "    ((0)\n"
        "        (DO COMPUTERS WORRY YOU)\n"
        "        (WHY DO YOU MENTION COMPUTERS)\n"
        "        (WHAT DO YOU THINK MACHINES HAVE TO DO WITH YOUR PROBLEM)\n"
        "        (DON'T YOU THINK COMPUTERS CAN HELP PEOPLE)\n"
        "        (WHAT ABOUT MACHINES WORRIES YOU)\n"
        "        (WHAT DO YOU THINK ABOUT MACHINES)))\n"
        "(COMPUTERS 50 (= COMPUTER))\n"
        "(DAD = FATHER DLIST(/ FAMILY))\n"
        "(DEUTSCH (= XFREMD))\n"
        "(DIT\n"
        "    ((0)\n"
        "        (IN WHAT WAY)\n"
        "        (WHAT RESEMBLANCE DO YOU SEE)\n"
        "        (WHAT DOES THAT SIMILARITY SUGGEST TO YOU)\n"
        "        (WHAT OTHER CONNECTIONS DO YOU SEE)\n"
        "        (WHAT DO YOU SUPPOSE THAT RESEMBLANCE MEANS)\n"
        "        (WHAT IS THE CONNECTION, DO YOU SUPPOSE)\n"
        "        (COULD THERE REALLY BE SOME CONNECTION)\n"
        "        (HOW)))\n"
        "(DONT = DON'T)\n"
        "(DREAM 3\n"
        "    ((0)\n"
        "        (WHAT DOES THAT DREAM SUGGEST TO YOU)\n"
        "        (DO YOU DREAM OFTEN)\n"
        "        (WHAT PERSONS APPEAR IN YOUR DREAMS)\n"
        "        (DON'T YOU BELIEVE THAT DREAM HAS SOMETHING TO DO WITH YOUR PROBLEM)\n"
        "        (NEWKEY)))\n"
        "(DREAMED = DREAMT 4 (= DREAMT))\n"
        "(DREAMS = DREAM 3 (= DREAM))\n"
        "(DREAMT 4\n"
        "    ((0 YOU DREAMT 0)\n"
        "        (REALLY, 4)\n"
        "        (HAVE YOU EVER FANTASIED 4 WHILE YOU WERE AWAKE)\n"
        "        (HAVE YOU DREAMT 4 BEFORE)\n"
        "        (=DREAM)\n"
        "        (NEWKEY)))\n"
        "(ESPANOL (= XFREMD))\n"
        "(EVERYBODY 2 (= EVERYONE))\n"
        "(EVERYONE 2\n"
        "    ((0 (* EVERYONE EVERYBODY NOBODY NOONE) 0)\n"
        "        (REALLY, 2)\n"
        "        (SURELY NOT 2)\n"
        "        (CAN YOU THINK OF ANYONE IN PARTICULAR)\n"
        "        (WHO, FOR EXAMPLE)\n"
        "        (YOU ARE THINKING OF A VERY SPECIAL PERSON)\n"
        "        (WHO, MAY I ASK)\n"
        "        (SOMEONE SPECIAL PERHAPS)\n"
        "        (YOU HAVE A PARTICULAR PERSON IN MIND, DON'T YOU)\n"
        "        (WHO DO YOU THINK YOU'RE TALKING ABOUT)))\n"
        "(FATHER DLIST(/NOUN FAMILY))\n"
        "(FEEL DLIST(/BELIEF))\n"
        "(FRANCAIS (= XFREMD))\n"
        "(HELLO\n"
        "    ((0)\n"
        "        (HOW DO YOU DO. PLEASE STATE YOUR PROBLEM)))\n"
        "(HOW (= WHAT))\n"
        "(I = YOU\n"
        "    ((0 YOU (* WANT NEED) 0)\n"
        "        (WHAT WOULD IT MEAN TO YOU IF YOU GOT 4)\n"
        "        (WHY DO YOU WANT 4)\n"
        "        (SUPPOSE YOU GOT 4 SOON)\n"
        "        (WHAT IF YOU NEVER GOT 4)\n"
        "        (WHAT WOULD GETTING 4 MEAN TO YOU)\n"
        "        (WHAT DOES WANTING 4 HAVE TO DO WITH THIS DISCUSSION))\n"
        "    ((0 YOU ARE 0 (*SAD UNHAPPY DEPRESSED SICK) 0)\n"
        "        (I AM SORRY TO HEAR YOU ARE 5)\n"
        "        (DO YOU THINK COMING HERE WILL HELP YOU NOT TO BE 5)\n"
        "        (I'M SURE ITS NOT PLEASANT TO BE 5)\n"
        "        (CAN YOU EXPLAIN WHAT MADE YOU 5))\n"
        "    ((0 YOU ARE 0 (*HAPPY ELATED GLAD BETTER) 0)\n"
        "        (HOW HAVE I HELPED YOU TO BE 5)\n"
        "        (HAS YOUR TREATMENT MADE YOU 5)\n"
        "        (WHAT MAKES YOU 5 JUST NOW)\n"
        "        (CAN YOU EXPLAIN WHY YOU ARE SUDDENLY 5))\n"
        "    ((0 YOU WAS 0)\n"
        "        (=WAS))\n"
        "    ((0 YOU (/BELIEF) YOU 0)\n"
        "        (DO YOU REALLY THINK SO)\n"
        "        (BUT YOU ARE NOT SURE YOU 5)\n"
        "        (DO YOU REALLY DOUBT YOU 5))\n"
        "    ((0 YOU 0 (/BELIEF) 0 I 0)\n"
        "        (=YOU))\n"
        "    ((0 YOU ARE 0)\n"
        "        (IS IT BECAUSE YOU ARE 4 THAT YOU CAME TO ME)\n"
        "        (HOW LONG HAVE YOU BEEN 4)\n"
        "        (DO YOU BELIEVE IT NORMAL TO BE 4)\n"
        "        (DO YOU ENJOY BEING 4))\n"
        "    ((0 YOU (* CAN'T CANNOT) 0)\n"
        "        (HOW DO YOU KNOW YOU CAN'T 4)\n"
        "        (HAVE YOU TRIED)\n"
        "        (PERHAPS YOU COULD 4 NOW)\n"
        "        (DO YOU REALLY WANT TO BE ABLE TO 4))\n"
        "    ((0 YOU DON'T 0)\n"
        "        (DON'T YOU REALLY 4)\n"
        "        (WHY DON'T YOU 4)\n"
        "        (DO YOU WISH TO BE ABLE TO 4)\n"
        "        (DOES THAT TROUBLE YOU))\n"
        "    ((0 YOU FEEL 0)\n"
        "        (TELL ME MORE ABOUT SUCH FEELINGS)\n"
        "        (DO YOU OFTEN FEEL 4)\n"
        "        (DO YOU ENJOY FEELING 4)\n"
        "        (OF WHAT DOES FEELING 4 REMIND YOU))\n"
        "    ((0 YOU 0 I 0)\n"
        "        (PERHAPS IN YOUR FANTASY WE 3 EACH OTHER)\n"
        "        (DO YOU WISH TO 3 ME)\n"
        "        (YOU SEEM TO NEED TO 3 ME)\n"
        "        (DO YOU 3 ANYONE ELSE))\n"
        "    ((0)\n"
        "        (YOU SAY 1)\n"
        "        (CAN YOU ELABORATE ON THAT)\n"
        "        (DO YOU SAY 1 FOR SOME SPECIAL REASON)\n"
        "        (THAT'S QUITE INTERESTING)))\n"
        "(I'M = YOU'RE\n"
        "    ((0 YOU'RE 0)\n"
        "        (PRE (YOU ARE 3) (=I))))\n"
        "(IF 3\n"
        "    ((0 IF 0)\n"
        "        (DO YOU THINK ITS LIKELY THAT 3)\n"
        "        (DO YOU WISH THAT 3)\n"
        "        (WHAT DO YOU THINK ABOUT 3)\n"
        "        (REALLY, 2 3)))\n"
        "(ITALIANO (= XFREMD))\n"
        "(LIKE 10\n"
        "    ((0 (*AM IS ARE WAS) 0 LIKE 0)\n"
        "        (=DIT))\n"
        "    ((0)\n"
        "        (NEWKEY)))\n"
        "(MACHINE 50 (= COMPUTER))\n"
        "(MACHINES 50 (= COMPUTER))\n"
        "(MAYBE (= PERHAPS))\n"
        "(ME = YOU)\n"
        "(MOM = MOTHER DLIST(/ FAMILY))\n"
        "(MOTHER DLIST(/NOUN FAMILY))\n"
        "(MY = YOUR 2\n"
        "    ((0 YOUR 0 (/FAMILY) 0)\n"
        "        (TELL ME MORE ABOUT YOUR FAMILY)\n"
        "        (WHO ELSE IN YOUR FAMILY 5)\n"
        "        (YOUR 4)\n"
        "        (WHAT ELSE COMES TO MIND WHEN YOU THINK OF YOUR 4))\n"
        "    ((0 YOUR 0)\n"
        "        (YOUR 3)\n"
        "        (WHY DO YOU SAY YOUR 3)\n"
        "        (DOES THAT SUGGEST ANYTHING ELSE WHICH BELONGS TO YOU)\n"
        "        (IS IT IMPORTANT TO YOU THAT 2 3)))\n"
        "(MYSELF = YOURSELF)\n"
        "(NAME 15\n"
        "    ((0)\n"
        "        (I AM NOT INTERESTED IN NAMES)\n"
        "        (I'VE TOLD YOU BEFORE, I DON'T CARE ABOUT NAMES - PLEASE CONTINUE)))\n"
        "(NO\n"
        "    ((0)\n"
        "        (ARE YOU SAYING 'NO' JUST TO BE NEGATIVE)\n"
        "        (YOU ARE BEING A BIT NEGATIVE)\n"
        "        (WHY NOT)\n"
        "        (WHY 'NO')))\n"
        "(NOBODY 2 (= EVERYONE))\n"
        "(NOONE 2 (= EVERYONE))\n"
        "(PERHAPS\n"
        "    ((0)\n"
        "        (YOU DON'T SEEM QUITE CERTAIN)\n"
        "        (WHY THE UNCERTAIN TONE)\n"
        "        (CAN'T YOU BE MORE POSITIVE)\n"
        "        (YOU AREN'T SURE)\n"
        "        (DON'T YOU KNOW)))\n"
        "(REMEMBER 5\n"
        "    ((0 YOU REMEMBER 0)\n"
        "        (DO YOU OFTEN THINK OF 4)\n"
        "        (DOES THINKING OF 4 BRING ANYTHING ELSE TO MIND)\n"
        "        (WHAT ELSE DO YOU REMEMBER)\n"
        "        (WHY DO YOU REMEMBER 4 JUST NOW)\n"
        "        (WHAT IN THE PRESENT SITUATION REMINDS YOU OF 4)\n"
        "        (WHAT IS THE CONNECTION BETWEEN ME AND 4))\n"
        "    ((0 DO I REMEMBER 0)\n"
        "        (DID YOU THINK I WOULD FORGET 5)\n"
        "        (WHY DO YOU THINK I SHOULD RECALL 5 NOW)\n"
        "        (WHAT ABOUT 5)\n"
        "        (=WHAT)\n"
        "        (YOU MENTIONED 5))\n"
        "    ((0)\n"
        "        (NEWKEY)))\n"
        "(SAME 10 (= DIT))\n"
        "(SISTER DLIST(/FAMILY))\n"
        "(SORRY\n"
        "    ((0)\n"
        "        (PLEASE DON'T APOLIGIZE)\n"
        "        (APOLOGIES ARE NOT NECESSARY)\n"
        "        (WHAT FEELINGS DO YOU HAVE WHEN YOU APOLOGIZE)\n"
        "        (I'VE TOLD YOU THAT APOLOGIES ARE NOT REQUIRED)))\n"
        "(THINK DLIST(/BELIEF))\n"
        "(WAS 2\n"
        "    ((0 WAS YOU 0)\n"
        "        (WHAT IF YOU WERE 4)\n"
        "        (DO YOU THINK YOU WERE 4)\n"
        "        (WERE YOU 4)\n"
        "        (WHAT WOULD IT MEAN IF YOU WERE 4)\n"
        "        (WHAT DOES ' 4 ' SUGGEST TO YOU)\n"
        "        (=WHAT))\n"
        "    ((0 YOU WAS 0)\n"
        "        (WERE YOU REALLY)\n"
        "        (WHY DO YOU TELL ME YOU WERE 4 NOW)\n"
        "        (PERHAPS I ALREADY KNEW YOU WERE 4))\n"
        "    ((0 WAS I 0)\n"
        "        (WOULD YOU LIKE TO BELIEVE I WAS 4)\n"
        "        (WHAT SUGGESTS THAT I WAS 4)\n"
        "        (WHAT DO YOU THINK)\n"
        "        (PERHAPS I WAS 4)\n"
        "        (WHAT IF I HAD BEEN 4))\n"
        "    ((0)\n"
        "        (NEWKEY)))\n"
        "(WERE = WAS (= WAS))\n"
        "(WHAT\n"
        "    ((0)\n"
        "        (WHY DO YOU ASK)\n"
        "        (DOES THAT QUESTION INTEREST YOU)\n"
        "        (WHAT IS IT YOU REALLY WANT TO KNOW)\n"
        "        (ARE SUCH QUESTIONS MUCH ON YOUR MIND)\n"
        "        (WHAT ANSWER WOULD PLEASE YOU MOST)\n"
        "        (WHAT DO YOU THINK)\n"
        "        (WHAT COMES TO YOUR MIND WHEN YOU ASK THAT)\n"
        "        (HAVE YOU ASKED SUCH QUESTIONS BEFORE)\n"
        "        (HAVE YOU ASKED ANYONE ELSE)))\n"
        "(WHEN (= WHAT))\n"
        "(WHY\n"
        "    ((0 WHY DON'T I 0)\n"
        "        (DO YOU BELIEVE I DON'T 5)\n"
        "        (PERHAPS I WILL 5 IN GOOD TIME)\n"
        "        (SHOULD YOU 5 YOURSELF)\n"
        "        (YOU WANT ME TO 5)\n"
        "        (=WHAT))\n"
        "    ((0 WHY CAN'T YOU 0)\n"
        "        (DO YOU THINK YOU SHOULD BE ABLE TO 5)\n"
        "        (DO YOU WANT TO BE ABLE TO 5)\n"
        "        (DO YOU BELIEVE THIS WILL HELP YOU TO 5)\n"
        "        (HAVE YOU ANY IDEA WHY YOU CAN'T 5)\n"
        "        (=WHAT)))\n"
        "(WIFE DLIST(/FAMILY))\n"
        "(WISH DLIST(/BELIEF))\n"
        "(WONT = WON'T)\n"
        "(XFREMD\n"
        "    ((0)\n"
        "        (I AM SORRY, I SPEAK ONLY ENGLISH)))\n"
        "(YES\n"
        "    ((0)\n"
        "        (YOU SEEM QUITE POSITIVE)\n"
        "        (YOU ARE SURE)\n"
        "        (I SEE)\n"
        "        (I UNDERSTAND)))\n"
        "(YOU = I\n"
        "    ((0 I REMIND YOU OF 0)\n"
        "        (=DIT))\n"
        "    ((0 I ARE 0)\n"
        "        (WHAT MAKES YOU THINK I AM 4)\n"
        "        (DOES IT PLEASE YOU TO BELIEVE I AM 4)\n"
        "        (DO YOU SOMETIMES WISH YOU WERE 4)\n"
        "        (PERHAPS YOU WOULD LIKE TO BE 4))\n"
        "    ((0 I 0 YOU)\n"
        "        (WHY DO YOU THINK I 3 YOU)\n"
        "        (YOU LIKE TO THINK I 3 YOU - DON'T YOU)\n"
        "        (WHAT MAKES YOU THINK I 3 YOU)\n"
        "        (REALLY, I 3 YOU)\n"
        "        (DO YOU WISH TO BELIEVE I 3 YOU)\n"
        "        (SUPPOSE I DID 3 YOU - WHAT WOULD THAT MEAN)\n"
        "        (DOES SOMEONE ELSE BELIEVE I 3 YOU))\n"
        "    ((0 I 0)\n"
        "        (WE WERE DISCUSSING YOU - NOT ME)\n"
        "        (OH, I 3)\n"
        "        (YOU'RE NOT REALLY TALKING ABOUT ME - ARE YOU)\n"
        "        (WHAT ARE YOUR FEELINGS NOW)))\n"
        "(YOU'RE = I'M\n"
        "    ((0 I'M 0)\n"
        "        (PRE (I ARE 3) (=YOU))))\n"
        "(YOUR = MY\n"
        "    ((0 MY 0)\n"
        "        (WHY ARE YOU CONCERNED OVER MY 3)\n"
        "        (WHAT ABOUT YOUR OWN 3)\n"
        "        (ARE YOU WORRIED ABOUT SOMEONE ELSES 3)\n"
        "        (REALLY, MY 3)))\n"
        "(YOURSELF = MYSELF)\n"
        "(MEMORY MY\n"
        "    (0 YOUR 0 = LETS DISCUSS FURTHER WHY YOUR 3)\n"
        "    (0 YOUR 0 = EARLIER YOU SAID YOUR 3)\n"
        "    (0 YOUR 0 = BUT YOUR 3)\n"
        "    (0 YOUR 0 = DOES THAT HAVE ANYTHING TO DO WITH THE FACT THAT YOUR 3))\n"
        "(NONE\n"
        "    ((0)\n"
        "        (I AM NOT SURE I UNDERSTAND YOU FULLY)\n"
        "        (PLEASE GO ON)\n"
        "        (WHAT DOES THAT SUGGEST TO YOU)\n"
        "        (DO YOU FEEL STRONGLY ABOUT DISCUSSING SUCH THINGS)))\n"
        "()\n";


    std::stringstream ss;
    ss.str(script_text);

    elizascript::script s;
    elizascript::read<std::stringstream>(ss, s);

    TEST_EQUAL(s.rules.size(), (size_t)68);
    TEST_EQUAL(to_string(s), script_text);

    elizalogic::tagmap tags(elizalogic::collect_tags(s.rules));
    TEST_EQUAL(tags.size(), (size_t)3);
    TEST_EQUAL(join(tags["BELIEF"]), "BELIEVE FEEL THINK WISH");
    TEST_EQUAL(join(tags["FAMILY"]), "BROTHER CHILDREN DAD FATHER MOM MOTHER SISTER WIFE");
    TEST_EQUAL(join(tags["NOUN"]), "FATHER MOTHER");


    // test [0, YOU, (*WANT NEED), 0] matches [YOU, NEED, NICE, FOOD]
    stringlist pattern;
    pattern.push_back("0");
    pattern.push_back("YOU");
    pattern.push_back("(*WANT NEED)");
    pattern.push_back("0");
    stringlist matching_components;
    TEST_EQUAL(elizalogic::match(elizalogic::tagmap(),
        pattern,
        elizalogic::split("YOU NEED NICE FOOD"),
        matching_components), true);
    TEST_EQUAL(matching_components.size(), (size_t)4);
    if (matching_components.size() == 4) {
        TEST_EQUAL(matching_components[0].empty(), true);
        TEST_EQUAL(matching_components[1], "YOU");
        TEST_EQUAL(matching_components[2], "NEED");
        TEST_EQUAL(matching_components[3], "NICE FOOD");
    }

    // test [0, 0, YOU, (*WANT NEED), 0] matches [YOU, WANT, NICE, FOOD]
    pattern.push_front("0");
    TEST_EQUAL(elizalogic::match(elizalogic::tagmap(),
        pattern,
        elizalogic::split("YOU WANT NICE FOOD"),
        matching_components), true);
    TEST_EQUAL(matching_components.size(), (size_t)5);
    if (matching_components.size() == 5) {
        TEST_EQUAL(matching_components[0].empty(), true);
        TEST_EQUAL(matching_components[1].empty(), true);
        TEST_EQUAL(matching_components[2], "YOU");
        TEST_EQUAL(matching_components[3], "WANT");
        TEST_EQUAL(matching_components[4], "NICE FOOD");
    }

    // test [1, (*WANT NEED), 0] matches [YOU, WANT, NICE, FOOD]
    pattern.pop_front();
    pattern.pop_front();
    pattern[0] = "1";
    TEST_EQUAL(elizalogic::match(elizalogic::tagmap(),
        pattern,
        elizalogic::split("YOU WANT NICE FOOD"),
        matching_components), true);
    TEST_EQUAL(matching_components.size(), (size_t)3);
    if (matching_components.size() == 3) {
        TEST_EQUAL(matching_components[0], "YOU");
        TEST_EQUAL(matching_components[1], "WANT");
        TEST_EQUAL(matching_components[2], "NICE FOOD");
    }

    // test [1, (*WANT NEED), 1] doesn't match [YOU, WANT, NICE, FOOD]
    pattern[2] = "1";
    TEST_EQUAL(elizalogic::match(elizalogic::tagmap(),
        pattern,
        elizalogic::split("YOU WANT NICE FOOD"),
        matching_components), false);

    // test [1, (*WANT NEED), 2] matches [YOU, WANT, NICE, FOOD]
    pattern[2] = "2";
    TEST_EQUAL(elizalogic::match(elizalogic::tagmap(),
        pattern,
        elizalogic::split("YOU WANT NICE FOOD"),
        matching_components), true);
    TEST_EQUAL(matching_components.size(), (size_t)3);
    if (matching_components.size() == 3) {
        TEST_EQUAL(matching_components[0], "YOU");
        TEST_EQUAL(matching_components[1], "WANT");
        TEST_EQUAL(matching_components[2], "NICE FOOD");
    }



    struct exchange {
        const char * prompt;    // input to ELIZA
        const char * response;  // output expected from ELIZA
    };
    const exchange conversation[] = {
        // --- exact conversation quoted in 1966 CACM article ---
        { "Men are all alike.",
          "IN WHAT WAY" },

        { "They're always bugging us about something or other.",
          "CAN YOU THINK OF A SPECIFIC EXAMPLE" },

        { "Well, my boyfriend made me come here.",
          "YOUR BOYFRIEND MADE YOU COME HERE" },

        { "He says I'm depressed much of the time.",
          "I AM SORRY TO HEAR YOU ARE DEPRESSED" },

        { "It's true. I am unhappy.",
          "DO YOU THINK COMING HERE WILL HELP YOU NOT TO BE UNHAPPY" },

        { "I need some help, that much seems certain.",
          "WHAT WOULD IT MEAN TO YOU IF YOU GOT SOME HELP" },

        { "Perhaps I could learn to get along with my mother.",
          "TELL ME MORE ABOUT YOUR FAMILY" },

        { "My mother takes care of me.",
          "WHO ELSE IN YOUR FAMILY TAKES CARE OF YOU" },

        { "My father.",
          "YOUR FATHER" },

        { "You are like my father in some ways.",
          "WHAT RESEMBLANCE DO YOU SEE" },

        // In the CACM article, the following prompt contained no comma.
        // Without the comma, this simulation responds "WHAT MAKES YOU
        // THINK I AM NOT VERY AGGRESSIVE BUT YOU THINK I DON'T WANT YOU TO
        // NOTICE THAT". I assume the comma got lost from the CACM article.
        { "You are not very aggressive, but I think you don't want me to notice that.",
          "WHAT MAKES YOU THINK I AM NOT VERY AGGRESSIVE" },

        { "You don't argue with me.",
          "WHY DO YOU THINK I DON'T ARGUE WITH YOU" },

        { "You are afraid of me.",
          "DOES IT PLEASE YOU TO BELIEVE I AM AFRAID OF YOU" },

        { "My father is afraid of everybody.",
          "WHAT ELSE COMES TO MIND WHEN YOU THINK OF YOUR FATHER" },

        { "Bullies.",
          "DOES THAT HAVE ANYTHING TO DO WITH THE FACT THAT YOUR BOYFRIEND MADE YOU COME HERE" },
        // --- end of 1966 CACM article conversation ---
    };

    /*  I believe that if this code reproduces the exact conversation
        published in the CACM article, using the mechanisms described in
        that article, it is not unreasonable to suppose it is a fairly
        accurate simulation of the original ELIZA.
    */
    for (const auto & exchg : conversation)
        TEST_EQUAL(eliza(s.rules, exchg.prompt), exchg.response);

}

}//namespace elizatest



// write given 's' to std::cout, followed by newline
void writeln(const std::string & s)
{
    if (true) {
        // for fun, output 's' as if ELIZA was an imperfect typist
        // N.B. this idea is NOT authentic to the 1966 ELIZA

        auto sleep = [](long ms) { std::this_thread::sleep_for(std::chrono::milliseconds(ms)); };
        auto chance = [](int n) { return rand() % n == 0; };
        static char fatfingers[] = { "SVVFRDFJUHJKNBIOWTAYICEZTX" };
        using std::cout; using std::flush; using std::endl;

        sleep(1000);
        for (const auto c : s) {
            if (chance(100) && 'A' <= c && c <= 'Z') {
                cout << fatfingers[c - 'A'] << flush;
                if (chance(2)) {
                    sleep(30);
                    cout << c << flush;
                    sleep(300);
                    cout << "\b \b" << flush;
                    sleep(30);
                    cout << "\b \b" << flush;
                }
                else {
                    sleep(300);
                    cout << "\b \b" << flush;
                }
                sleep(200);
            }
            cout << c << flush;
            sleep(10 + rand() % 50 + (chance(10) ? 200 : 0));
        }
        cout << endl;
    }
    else {
        // spit out all of 's' in an instant
        std::cout << s << std::endl;
    }
}



int main(int argc, const char * argv[])
{
    try {
        elizatest::test();

        if (argc != 2) {
            std::cerr
                << "Usage: " << argv[0] << " <scriptfile>\n"
                << "  where <scriptfile> is in original 1966 ELIZA script format\n";
            return EXIT_FAILURE;
        }

        std::ifstream script_file(argv[1]);
        if (!script_file.is_open()) {
            std::cerr << argv[0]
                << ": failed to open script file '" << argv[1] << "'\n";
            return EXIT_FAILURE;
        }

        elizascript::script s;
        elizascript::read<std::ifstream>(script_file, s);

        writeln(join(s.hello_message));
        for (;;) {
            std::cout << std::endl;
            std::string userinput;
            std::getline(std::cin, userinput);
            writeln(eliza(s.rules, userinput));
        }
    }
    catch (const std::exception & e) {
        std::cerr << "exception: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }
    catch (...) {
        std::cerr << "exception" << std::endl;
        return EXIT_FAILURE;
    }
}

// (The goal was to make only a minimum viable accurate simulation
// of the original 1966 ELIZA rather than a polished product.)
