SELECT GroupName, AvgMark
FROM Groups AS G
LEFT JOIN (
    SELECT avg(cast(Mark AS REAL)) AS AvgMark, GroupId 
    FROM Marks
    NATURAL JOIN Students
    GROUP BY GroupId
) AS M ON G.GroupId = M.GroupId 