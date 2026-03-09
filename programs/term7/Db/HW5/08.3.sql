SELECT GroupName, SumMark
FROM Groups AS G
LEFT JOIN (
    SELECT sum(Mark) AS SumMark, GroupId 
    FROM Marks
    NATURAL JOIN Students
    GROUP BY GroupId
) AS M ON G.GroupId = M.GroupId 