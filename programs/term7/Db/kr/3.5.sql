update Runs
set Accepted = 1
where RunId in (
    select r.RunId
    from Runs as r
    where r.SubmitTime in (
        select MAX(rr.SubmitTime)
        from Runs as rr
        where rr.Accepted = 0
        and r.SessionId = rr.SessionId
    )
    and r.Accepted = 0
);