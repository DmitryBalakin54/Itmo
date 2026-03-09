update Runs
set Accepted = 1
where RunId in (
    select r.RunId
    FROM Runs as r
    where r.SubmitTime in (
        select MAX(rr.SubmitTime)
        from Runs as rr
        where r.SessionId = rr.SessionId
    )
);