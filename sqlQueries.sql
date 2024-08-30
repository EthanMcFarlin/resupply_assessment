/*
====================================================
[Query 1]
====================================================
*/


/*
Goal: Calculate a hollistic measure of overall completion 
*/

WITH overallCompletion AS (
    SELECT 
        COUNT(*) AS total_donations,
        COUNT(CASE WHEN hauler_status = 'completed' THEN 1 END) AS completed_donations
    FROM donations
)
SELECT 
    total_donations,
    completed_donations,
    (completed_donations * 100.0 / total_donations) AS completion_percentage
FROM overallCompletion;

/*
Results:
- total_donations: 101584
- completed_donations: 76460
- completion_percentage: 75.26%
*/


/*
====================================================
[Query 2]
====================================================
*/


/*
Goal: Segment completion percentage by market
*/

WITH completedDonations AS (
    SELECT 
        donation_id,
        market_name,
        SUM(CASE WHEN hauler_status = 'completed' THEN 1 ELSE 0 END) AS completedDonations,
        COUNT(*) AS totalDonations
    FROM donations
    GROUP BY market_name
),
completionPercentage AS (
    SELECT 
        market_name,
        completedDonations,
        totalDonations,
        (completedDonations * 100.0 / totalDonations) AS completionPercentage
    FROM completedDonations
)
SELECT 
    market_name,
    completedDonations,
    totalDonations,
    completionPercentage
FROM completionPercentage
ORDER BY completionPercentage DESC;


/*
====================================================
[Query 3]
====================================================
*/


/*
Goal: Join 'donations' and 'events' tables and show some basic stats
*/

WITH donationStats AS (
    SELECT 
        donation_id,
        COUNT(*) AS totalEvents,
        COUNT(CASE WHEN event_type = 'partner_complete' THEN 1 END) AS completionEvents,
        MAX(event_timestamp) AS lastEventTime
    FROM events
    GROUP BY donation_id
),
completed_donations AS (
    SELECT 
        d.donation_id,
        d.submitted_at,
        d.donation_date,
        d.pickup_address_zip_code,
        d.pickup_address_state,
        d.payment_completed,
        d.hauler_status,
        d.assigned_hauler_name,
        d.assigned_charity_name,
        d.market_name,
        ds.totalEvents,
        ds.completionEvents,
        ds.lastEventTime
    FROM donations d
    LEFT JOIN donationStats ds ON d.donation_id = ds.donation_id
)
SELECT 
    donation_id,
    submitted_at,
    donation_date,
    pickup_address_zip_code,
    pickup_address_state,
    payment_completed,
    hauler_status,
    assigned_hauler_name,
    assigned_charity_name,
    market_name,
    totalEvents,
    completionEvents,
    lastEventTime,
    (CASE 
        WHEN hauler_status = 'completed' THEN 'Yes'
        ELSE 'No'
    END) AS isCompleted
FROM completed_donations;

