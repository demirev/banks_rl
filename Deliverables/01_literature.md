-   [Agent-Based Models in Economics](#agent-based-models-in-economics)
-   [ABM Models of the Banking
    Sector](#abm-models-of-the-banking-sector)
-   [Reinforcement Learning in
    Economics](#reinforcement-learning-in-economics)
-   [Q Learning](#q-learning)
-   [References](#references)

The paper hereafter attempts to model the banking sector as a collection
of autonomous actors, each making profit- or utility-maximizing
decisions, based on mechanisms for learning optimal behavior and
adapting to a changing environment. As such it falls under the category
of agent-based computational models. This short document gives a brief
overview of the field - both in terms of applications of agent based
modelling to banking, as well as the tools of adaptive optimization in
general economics. It also makes a quick survey of recent advances in
the theory and application of autonomous agents that are to be employed
in the paper.

Agent-Based Models in Economics
===============================

Agent-based economic models trace their roots to the late sixties, when
Nobel laureatte Thomas Schelling (1969) developed a cellular automaton
to model racial segregation in the market for residential properties. In
this models agents of two colors chose where 'to live' on a two
dimensional grid based on simple preferences regarding their neighbours.
The paper demonstrated, that segragation emerges naturally even when
agents have no bias against the opposite color. He later went on to
write the book "Micromotives and macrobehavior" (T. C. Schelling 1978)
considered to be a seminal work in the field.

With the advent of cheap computational resources ABMs became more and
more complex. Epstein and Axtell (1996) were among the first to
introduce trade in an agent-based setting. Their famous model
"Sugarscape", again based on a 2-D cellular automata, consists of
multiple agents traversing a landscape which produces two resources -
'sugar' and 'spice'. By giving the agents very simple rules of behavior
and the ability to trade, Epstein and Axtell manage to reproduce many
features of real-life markets, like price cycles and severe wealth
inequality.

More recently, agent-based models have been gaining support as a
promising alternative to standard fixed-point-based models of economic
activities. Proponents (see for example Howitt (2008)) argue that ABMs
allow for circumventing some of the problematic assumptions that lay
below contemprorary economic theory. Namely, agent-based economies can
be completely decentralized, removing the need for the abstraction of
the Walrasian auctioneer and the assumption of equilibrium knowledge.
Recent developments in the study of the complexity of game theory
(Daskalakis, Goldberg, and Papadimitriou 2009), that show that finding a
Brouwer-fixed point is generally not feasible in polynomial time (thus
discrediting the validity of Walrasian equilibrium and Nash equilibrium
in general) have given further weight to such claims.

ABM Models of the Banking Sector
================================

With the unexpected series of bank failures in the US that kicked off
the 2008 global economic downturn, the economics profession became under
pressure to develop new better models of the banking sector. Naturally,
many papers using an agent-based methodology took up the tasks. Focusing
on adaptive behavior and inter-bank linkages, these papers have tried to
create models that can explain widespread financial crisis.

Most recently, Chan-Lau (2017) develops a modular agent-based model of
the financial sector based around three agents - banks, savers, and
loans (albeit savers and loans do not really make autonomous decisions
in his formulation). The model is characterized by a complex behavior
system for banks, completed by divident policy, portfolio rebalancing,
and a set of regulatory requirements. The risk of a bank run in the
model is introduced through a simple exogonous process at the savers'
level. Chan-Lau allows for interbank loans for banks with excess
reserves to be able to loan to banks with insufficient reserves. This in
turn introduces a degree of interlinkage across institutions that act as
a conduit for propogation of bank failures. Banks in the model also have
to make sure they provision for expected losses, have sufficient levels
of capital adequacy, and abide to a required minimal reserve ratio.
Having those regulatory constraints in place allows the analyst to
explore how changing them affects the health of the simulated economy -
a feature this paper will try to emulate.

Ismail (2012) models the emregence of a banking sector in an
cellular-automata-based artificial economy. In his model there are two
type of investors - patient and impatient, with the imaptient preferring
to liquidate their investment earlier, and the patient preferring to
wait until maturity. Types are individually observed and subject to
shocks. Ismail allows agents to trade with their neighbours in the CA.
The need for banks arrises as not all agent in the automata who want to
trade are able to find traidng partners. In Ismail's model bank creation
is endogenized, as each investor can choose whether to become an
inetmeidary or not. Agents' behavior learning process centers around
predicting their type shocks, with prediction based on several 'rules',
whose weight in decision making is updated in a
reinforcement-learning-like fashion. Banks update a single parameter
(expected proportion of impatient investors) using exponential
smoothing.

Grilli, Tadeschi and Gallegati (2014) build on top of an agent-based
model by Delli Gatti et al (2005) to model a system of interconnected
banks. In their model firms invest facing an uncertain future sale
price, and use both borrowed funds and retained earning to fincance
their projects. As future income for the firm is uncertain there is a
possibility of firm default and hence credit loss for the bank. The loss
for the bank can be high enough to cause filure. On the other hand
Grilli et al also allow for interbank loans, leading to propagation of
bank failures. The connections between banks are a key object of
interest for this paper, with the authors treating the interbank market
as an Erdos-Renyi graph. The model manages to produce endogenous
business cycle due to increasing leverage in the boom and decrease in
credit availability in the bust.

Another paper that demonstrates the emergence of levarege-driven
business cycles is due to Aymanss and Farmer (2015). In their framework,
boundedly rational investors (banks) follow a value-at-risk investments
rule and choose investments in a stock market. In this models banks
estimate expected volatility and asset covariance using historical data.
This leads to lowering levarage when recent volatility is low and vice
versa. Aymanss and Farmer demonstrate that this has a strong
pro-cyclical effect, as small changes in perceived volatility in the
boom part of the cycle lead to large changes in banks' target levarage,
triggering fire sales.

Agent-based models of the financial sector have also find succesfull
applications in the private secotr. For example Geanakoplos (2012) has
developed over the course of more than a decade a detailed model of the
mortgage market in Washington DC that was used by financial firms on
Wall Street to forecast loan repayments. Geanakoplos points out the
usefullness of the agent-based approach when it comes to counterfactual
reasoning, as implementing alternative policies and what-if scenarios is
seamless in an agent-based setting. He also ellaborates on approaches to
fitting ABMs to real world data.

Ashraf Gershman and Howitt (2011) study the role of banks as stabilizers
in an economy of heterogenous agents. They argue that firms (and
retailers, wholesalers and other commercial entities) have a role of
intermedieries in the economy, essentially serving as a Walrasian
auctioneer that coordinates individual agent's behavior. They then point
out that, as banks are important to support and sustain the existence of
firms by providing credit, the finnacial sector can act as a stabilizing
force in the business cycle (a sort of financial stabilizer in contrast
to the traditional notion of financial acceleration). They use an
agent-based macro model, while trying to reconcile individual behavior
rules with standard practices from New Keynsian DSGEs.

Reinforcement Learning in Economics
===================================

Outside agent-based models, adaptive behavior of economic agents usually
falls withing the domain of microeconomics (see e.g. Fudenberg and
Levine (2016) for a review). Even though not considered part of the same
strand of literature as ABMs, some notable papers have succesfully
employed methods from reinforcement learning to modeling economic
interatctions. As this paper borrows heavily from research in that
field, it is worth mentioning some of them.

Even though the details of a working implementation are notably complex,
the basic idea behind reinforcement learning is simple - an agent adapts
her behavior by doing actions that lead to positive consequences more
often than actions that lead to negative consequences. This idea is
illustrated by Roth and Erev (1995), who use a simple reinforcement
learning framework to model three simple repeated games. The agents in
all three games have a discrete set of actions (something that will aslo
be the case in this paper) and each of them keep a 'propensity' for each
action (in RL terminology this is the q-value of the action). Roth and
Erev then go on to show that this simple model predicts behavior that
converges (or doesn't covnerge) to the theoretical pure strategy Nash
Equilibrium in a way, that is strikingly similar to observed
experimental behavior.

In a second paper Erev and Roth (1998) expand their model by gathering
data on over a number of experiments, involving repaeted games with
mixed strategy Nash equilibria. Their reinforcement learning model
includes what they call 'forgetting' or 'recency' (a tendency to only
slowly adjust q-values with experience that may help track unstationary
environments - akin to fixed or decreasing update steps in Q-learning)
as well as experimentation (akin to epsilon greedy policies in the
jargon of reinforcement learning). Again they manage to demonstrate an
impressive fit to experimental data with only minimal calibration of
parameters.

More recently Fudenberg and Peysakhovich (2014) build on a similar idea
of recency to model a simple assymetric information (or 'lemons') game.
In it agents are modelled as employing temporal difference learning (see
Sutton and Barto 2018) to adjust their trading bids. In TD learning
agents update their q-values after each action based on imperfect
assesments of the value of the state they end up with (known as
'bootstraping' in RL terminology). It relies on Bellman errors
(difference between expected state value before and after interacting
with the enviroment) to update agents' policy. Similar to Roth and Erev,
Fudenberg and Peysakhovich go on to demonstrate their model's predictive
powers by succesfully matching simulated and experimental outcomes.

Q Learning
==========

The current paper aims to build on the tradition of agent-based models
by enriching them with a more sofisticated decision making process for
each agent (employing tecniques from reinfrocement learning). In doing
so it will introduce some methods for approximate optimization that
originate withing computer science, robotic control and artificial
intelligence but have so far seen limited use in social sciences.

The fundamental learning algorithm that is to be employed by the agents
in this model is known as Q-Learning. The *Q* in the name comes from the
traditional notation for a action-value function in dynamic programming,
where *Q*(*s*, *a*) usually denotes the expected discounted stream of
future rewards (where rewards can be profit, utility, cash flows or
anything else depending on the application), following state *s* in
which action *a* is taken.

Q-learning relates to a general algorithm of finding an approximate
solution to the dynamic programming program, given by the Bellman
equation:

*Q* \* (*s*, *a*)=*m**a**x*<sub>*a*</sub>*u*(*s*, *a*)+*E*<sub>*s*</sub>\[*V* \* (*s*′)\]

Where

*V* \* (*s*′) = *m**a**x*<sub>*a*</sub>*Q*(*s*′,*a*′)

With *s*′ being the next state, and *a*′ being the actions available at
*s*′.

Whereas in dynamic programming the problem can be solved levereging the
knowledge of system dynamics (e.g. by general policy-value iteration),
this is unfeasible in our framework (as it is in most real-world
applications), since the dynamics of the system are complex and
generally unknown by the agent. Thus the agent has to rely on
approximate solutions. The details are laid out in the section on agent
behavior and a comprehensive reference is Sutton and Barto (2018).

Some of the practical aspects of the implementation are influenced by a
seminal paper by Mihn et al. (2013), where deep Q networks (a main tool
of this paper) were introduced. One of the only papers so far to apply
deep Q networks to an economic problem is Leibo (2017) who uses them to
model sequential social dilemmas. In this paper agents optimize using
DQNs in two two-player computer games (“gathering” and “wolf-pack”). By
varying the payoff structure the authors manage to recreate several
famous game theoretic strategic interactions – prisoners’ dilemma, stag
hunt, and chicken – and show that the agents learn policies similar to
the ones prescribed by game theory.

References
==========

Ashraf, Quamrul, Boris Gershman, and Peter Howitt. 2011. “Banks, Market
Organization, and Macroeconomic Performance: An Agent-Based
Computational Analysis.” Working Paper 17102. Working Paper Series.
National Bureau of Economic Research.
doi:[10.3386/w17102](https://doi.org/10.3386/w17102).

Aymanns, Christoph, and J. Doyne Farmer. 2015. “The Dynamics of the
Leverage Cycle.” *Journal of Economic Dynamics and Control* 50: 155–79.
doi:[https://doi.org/10.1016/j.jedc.2014.09.015](https://doi.org/https://doi.org/10.1016/j.jedc.2014.09.015).

Chan-Lau, Jorge. 2017. “ABBA: An Agent-Based Model of the Banking
System” 17 (January): 1.

Daskalakis, Constantinos, Paul W. Goldberg, and Christos H.
Papadimitriou. 2009. “On the Complexity of Computing Nash Equilibrium.”
*SIAM Journal of Computing* 39: 195–259.
doi:[10.1137/070699652](https://doi.org/10.1137/070699652).

Epstein, Joshua M., and Robert Axtell. 1996. *Growing Artificial
Societies: Social Science from the Bottom up*. 1st ed. Vol. 1. The MIT
Press. <https://EconPapers.repec.org/RePEc:mtp:titles:0262550253>.

Erev, Ido, and Alvin Roth. 1998. “Predicting How People Play Games:
Reinforcement Learning in Experimental Games with Unique, Mixed Strategy
Equilibria.” *American Economic Review* 88 (4): 848–81.
<https://EconPapers.repec.org/RePEc:aea:aecrev:v:88:y:1998:i:4:p:848-81>.

Fudenberg, Drew, and David K. Levine. 2016. “Whither Game Theory?
Towards a Theory of Learning in Games.” *Journal of Economic
Perspectives* 30 (4): 151–70.
doi:[10.1257/jep.30.4.151](https://doi.org/10.1257/jep.30.4.151).

Fudenberg, Drew, and A Peysakhovich. 2014. “Recency, Records and Recaps:
Learning and Non-Equilibrium Behavior in a Simple Decision Problem.”
Working Paper 167691. Harvard University OpenScholar.
<https://ideas.repec.org/p/qsh/wpaper/167691.html>.

Gatti, Domenico Delli, Corrado Di Guilmi, Edoardo Gaffeo, Gianfranco
Giulioni, Mauro Gallegati, and Antonio Palestrini. 2005. “A new approach
to business fluctuations: heterogeneous interacting agents, scaling laws
and financial fragility.” *Journal of Economic Behavior and
Organization* 56 (4): 489–512.
<https://ideas.repec.org/a/eee/jeborg/v56y2005i4p489-512.html>.

Geanakoplos, John, Robert Axtell, J. Farmer, Peter Howitt, Benjamin
Conlee, Jonathan Goldstein, Matthew Hendrey, Nathan M. Palmer, and
Chun-Yi Yang. 2012. “Getting at Systemic Risk via an Agent-Based Model
of the Housing Market.” Cowles Foundation Discussion Papers 1852. Cowles
Foundation for Research in Economics, Yale University.
<https://EconPapers.repec.org/RePEc:cwl:cwldpp:1852>.

Grilli, Ruggero, Gabriele Tedeschi, and Mauro Gallegati. 2014. “Bank
Interlinkages and Macroeconomic Stability.” *International Review of
Economics & Finance* 34 (C): 72–88.
<https://EconPapers.repec.org/RePEc:eee:reveco:v:34:y:2014:i:c:p:72-88>.

Howitt, Peter. 2008. “Macroeconomics with Intelligent Autonomous
Agents.” *Macroeconomics in the Small and the Large: Essays on
Microfoundations, Macroeconomic Applications and Economic History in
Honor of Axel Leijonhufvud*.

Ismail, Omneia R.H. 2012. “AN Agent–BASED Computational Model for Bank
Formation and Interbank Networks,” July.

Leibo, Joel Z., Vinicius Zambaldi, Marc Lanctot, Janusz Marecki, and
Thore Graepel. 2017. “Multi-Agent Reinforcement Learning in Sequential
Social Dilemmas.”

Mnih, Volodymyr, Koray Kavukcuoglu, David Silver, Alex Graves, Ioannis
Antonoglou, Daan Wierstra, and Martin Riedmiller. 2013. “Playing Atari
with Deep Reinforcement Learning.”

Roth, Alvin, and Ido Erev. 1995. “Learning in Extensive-Form Games:
Experimental Data and Simple Dynamic Models in the Intermediate Term.”
*Games and Economic Behavior* 8 (1): 164–212.
<https://EconPapers.repec.org/RePEc:eee:gamebe:v:8:y:1995:i:1:p:164-212>.

Schelling, Thomas. 1969. “Models of Segregation.” *American Economic
Review* 59 (2): 488–93.
<https://EconPapers.repec.org/RePEc:aea:aecrev:v:59:y:1969:i:2:p:488-93>.

Schelling, Thomas C. 1978. *Micromotives and Macrobehavior*. W. W.
Norton & Company.

Sutton, Richard S., and Andrew G. Barto. 2018. *Reinforcement Learning -
an Introduction, 2nd Edition*. Final Draft.
<http://www.incompleteideas.net/book/the-book.html>.
