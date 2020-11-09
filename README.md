- The basic structure was provided by the git repository:  https://github.com/scafi/learning-scafi-alchemist


- In src / main / scala I created the it.unito.planningFC package containing the PlanningFcApp.scala file. It is the file to be executed that provides the inputs to the simulation and starts the simulation with the startSimulationScenarioTaxi method, to which we supply the appropriate parameters (pathPlan, namePlan, taxis, passengers).
The startSimulationScenarioTaxi method is located in the Simulator object and starts the simulation of the Taxi scenario.
It is structured as follows: setupSimultation, then in loop it executes and manages each action of the FEL (Future Event List), which serves to guide the simulation, and finally ends the simulation.
Based on the actions of the FEL, the actions of the Plan are implemented, and they will have effects on the actors (Taxis and Passengers), to whom the actions are communicated via messages.