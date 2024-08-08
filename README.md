# Comparative Study of the Impact of Aircraft Boarding Processes on Aircraft Boarding Time Using Simulation Modeling

## Introduction
Every passenger has witnessed or experienced firsthand the stressful moments in airfare, especially during the boarding and deboarding processes. Regardless of the airline carrier, the wait time in front of the gate at the jet bridge is what feels like an eternity and the gate agents are gatekeeping the passengers from getting to their destination. The seemingly inefficient boarding process and the wait time are major contributing factors to customer satisfaction for airliners, according to Business Insider (Giacobone). Many airliners employ a tiered boarding system that can lead to envy among the passengers who may be lower in airline status. Additionally, it is not surprising that customers complain that their boarding experience is not personalized enough and that they are being herded onto the plane in this age of customization and world of heightened UX/UI. Furthermore, long wait times can lead to stress due to the cascading nature of the airline industry: a travel delay can derail the rest of the trip. 

In addition to passenger experience, shorter boarding processes benefit the airliners as shorter turnaround times prevent delays and avoid losing slots. Slots are take-off times assigned to aircraft by the traffic control. Delays in boarding can lead to aircraft missing their take-off times. As a result, aircraft must reapply for a slot (Coppens). 

### Problem Definition and Objective
In hopes of reducing the overall turnaround time for aircraft, airlines have adopted a variety of boarding strategies for the millions of passengers worldwide who experience the process of boarding each day. To optimize such a process, the initial step is to understand the overall passenger boarding process. The conventional boarding process illustrates the process of passengers entering the aircraft and all additional activities that define the boarding time as shown in the developed activity flow diagram in Figure 1. 

However, according to our definition, the boarding time is the time between the first passenger that steps into the jet bridge and the last passenger that is seated in their assigned seat on the aircraft. Consequently, the processes that occur before the jet bridge entrance, such as the boarding gate check, are not part of the boarding processes according to the team’s definition because passengers would have already lined up at the gate ahead of time, so there is no arrival time. Reductions in boarding time would significantly improve passenger satisfaction and yield economic benefits from more efficient application of aircraft and airport infrastructure. However, reducing boarding time can be a challenging and strenuous process, largely because it is directly dependent on the passengers. As passengers carry varying amounts of luggage and potentially travel with children, efficient and successful coordination of these factors is difficult, commonly resulting in a delayed boarding process depending on the enforced boarding strategy. However, in addition to various airlines employing different boarding processes, the outstanding problem is that traditional boarding strategies are unorganized and highly prone to passenger congestion, which primarily upsets the customer experience and decreases operational efficiencies. 

As every minute saved in boarding time is economically significant to airlines, a standard and optimal boarding procedure can greatly improve and benefit customer satisfaction, cabin crew coordination, aircraft scheduling, and profit margins. For the project, the team will specifically conduct a comparative analysis and focus on the effect of various boarding procedures on the overall boarding time. This study aims to determine the most appropriate and applicable boarding strategy that optimally minimizes boarding time to recommend as a standard practice for all airlines and to confront the conventional problem of inefficient aircraft boarding processes and lengthy boarding times. 

## Simulation Modeling
Our simulation model will focus on three boarding or seating processes, specifically Random Boarding with assigned seats, Back-to-Front zones, and WILMA, for the Airbus A321 aircraft, the most common small aircraft for domestic flights. This section describes the model setup and the input breakdown in detail.

### Assumptions and Limitations
Before simulating, several assumptions are made to simplify the system. Our assumptions can be broken down into 3 areas: modeled boarding process, plane settings, and passenger boarding behavior and related distributions.

#### Modeled Boarding Process
Realistically, the entire boarding process includes the queue before ticket scanning, the scanning process, the queue in the jet bridge and into the plane, and the seating process that includes storing carry-on luggage. The scanning would stop if the queue is at capacity and would resume when the queue starts to decrease. However, there would not exist a dramatically long wait time because the speed at which the queue is replenished is much greater than that of the reduction of the queue when passengers become seated. Based on an United Airlines (UA) presentation, in IOE 500 (IOE Master’s Seminar), by Simon Banks, an UA Senior Manager in Operations Strategy & Analytics the gate reader scans at a rate of 9 passengers per minute while the aircraft entry rate is around 7.5 passengers per minute, indicating a holdup from the line in aircraft waiting to be seated. Since seating is the bottleneck of the entire boarding process, this simulation defines boarding time as the time between the first passenger that is admitted at the boarding gate and the last passenger that is seated in their seat as mentioned previously.

To streamline the focus on simulating boarding time, the scanning queue, scanning distribution, queue capacity from the jet bridge to the plane, and the number of passengers in that queue are deemed irrelevant. Consequently, considerations regarding walking time when people are in the jet bridge and distance in the aisle have been omitted from the analysis. These factors have minimal impact, as the walk time in the jet bridge primarily affects the sitting time for the initial passengers, given that a queue usually forms in the jet bridge thereafter. Therefore, we focus solely on the walking time of the first person boarding the business class and the economy class as they proceed to their seat. The walking time on the aisle for individuals in the queue is deemed negligible, as it can take place simultaneously while previous passengers are being seated. Our assumption hinges on the belief that walking time is significantly less influential compared to the time spent seating and storing items. For modeling purposes, we made certain assumptions: a jet bridge length of 20 meters, seating rows spaced 1 meter apart, and individuals in the queue positioned one row apart from each other. Additionally, we assumed a walking speed of 1.34 m/s (CDC).

#### Plane Settings
As mentioned, the simulation is focused on the Airbus A321 aircraft and will use Delta Air Lines’ seating configuration for business class and economy class. The aircraft setting will have:
- 38 rows total
- 5 rows of business class with 4 seats in each row, 2 on each side of the aisle
- 33 rows of economy class with 6 seats in each row, 3 on each side of the aisle

It is also assumed that the plane will have enough overhead space for each passenger to store one item overhead. This is a reasonable assumption since the Federal Aviation Administration regulates the size and weight of carry-on luggage and most airlines, Delta Air Lines included, specify that passengers only bring one carry-on luggage in addition to a personal item. Note that some ultra-low cost carriers such as Spirit Airlines and Frontier Airline only allow one personal item and a carry-on would be an add-on purchase; Hence, we generated distributions under the assumption that 70% of the time, individuals carry a carry-on item. In addition to plane configuration, the model will assume that flights are fully booked, carrying 218 passengers in total. In other words, the boarding process begins after the first boarding pass has been scanned, i.e. the first passenger has begun boarding, and ends when the 218th passenger is seated.

#### Passenger Boarding Behavior and Distributions
Passenger behavior and preferences are factors in the boarding process that are difficult to measure and quantify. The study of behavior regarding boarding preferences is not within the scope of this project. To generalize behaviors, the model assumes that passengers abide by their boarding zones and will immediately queue to board once the zone is called up. This model does not consider passengers that will wait to queue later during other boarding zones’ timeframe.
During the boarding process, each passenger has to perform two actions (or two processes): storing the luggage (if they have a carry-on item) and sitting down. To simplify behavior, passengers with carry-on luggage will store it in the overhead compartment. Each passenger will carry their personal item while being seated and will not store their personal item overhead. These two actions will take different amounts of time, thus two separate distributions are defined below:
- Distribution for luggage storage: For the action of storing a luggage, the model assumes an exponential distribution with rate of 0.1 per second and mean of 10 seconds. A right-skewed distribution like exponential distribution is able to capture the slightly decreasing stowing efficiency as the plane fills. It is able to capture how it takes less time to store a luggage when there are less passengers on board and more time to store while the flight is full. Using an exponential distribution produces positive numbers with an average around 10 seconds, accounting for how most will be able to store luggages at a similar rate with a few outliers having to search or adjust for space in the overhead compartment. This assumption generalizes: 1) the physical abilities to handle a carry-on, 2) the increasing interference caused by stowing luggage the more luggage a person carries (however, we limit our passengers to carrying at most one carry-on luggage), and 3) the larger outliers of time taken to search for space or even move backwards, which could take a significant amount of time for passengers that boarded later as space fills up.
- Distribution for seating: Creating an exact distribution for such a hypothetical scenario would be challenging because there are many variables to consider, including the physical ability of the people involved, the space available, and whether there is luggage in the way under the seats. Additionally, it is generally socially unacceptable to climb over someone. Realistically, passengers who are already in their seats and blocking other passengers would stand up and step into the aisle to let someone pass. To model this, any right-skewed distribution could work well with seating time distributions such as Gamma or Exponential distributions, which account for the reality that most people are able to move out rather quickly. However, due to other factors, there will be some slower outliers. This data is difficult to collect and is not readily available in the literature. However, based on anecdotal evidence, the estimated rate to be seated is 0.7 per second (expected time in seconds is 1.43 seconds) if no one is blocking, 0.1 per second (expected time in seconds is 10s) if one person is blocking, and 0.05 per second (expected time in seconds is 20s) if two people are blocking. This simulation will assume exponential distribution with the respective rates mentioned.

### Cases
An efficient boarding process is a fundamental and critical factor in ensuring fast turnaround times and upholding passenger satisfaction and the overall success of airports and airlines. A boarding strategy determines how passengers in different cabin classes, entering the airplane sequentially one after another, are arranged across the seats inside the aircraft. To address the proposed problem, the team evaluated the efficiencies of three different existing boarding strategies, including Back-to-Front, Window Middle Aisle (WILMA), and Random. 

#### Back-to-Front Boarding Strategy
This boarding process is known as the “traditional” boarding method. Once business class passengers are boarded first, economy class passengers are boarded from the back (last) row to the front (first) row of the aircraft. This strategy is easy to implement but is vulnerable to inefficiencies due to congestions occurring while boarding rows.

#### WILMA Boarding Strategy
Boarding business class passengers first, economy class passengers are then boarded in the following order: those assigned to the windows seats board first, followed by those assigned to the middle seats, and then those assigned to the aisle seats. Easy to implement, this procedure reduces passenger interference caused by loading luggage, passengers colliding with each other while walking down the aircraft aisle, and already-seated passengers having to get up from their seats and step into the aisle to yield to other passengers with a more inner row seat.

#### Random Boarding Strategy (with Assigned Seats)
With this boarding process, each passenger is assigned a specific seat. Once business class passengers are boarded first, economy class passengers line up at the gate counter and are admitted into the aircraft in the order they arrive. Although this procedure is easy to implement, it is highly vulnerable to inefficiencies as a result of congestions occurring while boarding.
