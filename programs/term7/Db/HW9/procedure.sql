-- Нужно для хеширования пароля.
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Функция для нахождения пользователя по его id и паролю, возвращает true если
-- есть пользователь с таким паролем, иначе false, пароль хранится захешированным.
CREATE OR REPLACE FUNCTION FindUserByPass(user_id integer, user_pass varchar(40))
RETURNS boolean
LANGUAGE plpgsql AS $$
BEGIN
    RETURN EXISTS (
        SELECT 1
        FROM Users AS U
        WHERE U.UserId = user_id
        AND crypt(user_pass, U.PassHash) = U.PassHash
    );
END;
$$;

-- Функция возвращающая true если пользователь с данным id это админ,
-- иначе false. Админ это пользователь имеющий право на исполнение
-- ManageFlight(UserId, Pass, FlightId, SellAllowed, ReservationAllowed)
-- и CompressSeats(UserId, Pass, FlightId).
CREATE OR REPLACE FUNCTION IsAdmin(user_id integer)
RETURNS boolean
LANGUAGE plpgsql AS $$
BEGIN
    RETURN EXISTS (
        SELECT 1
        FROM Users
        WHERE UserId = user_id
        AND IsAdmin = true
    );
END;
$$;

-- Функция которая возвращает true если в момент времени curr_time можно купить
-- билет на рейс flight_id, иначе возвращает false. Билет нельзя купить если
-- до вылета рейса осталось менее 3 часов или SellAllowed = false.
CREATE OR REPLACE FUNCTION CanBuyFromNow(flight_id integer, curr_time timestamp)
RETURNS boolean
LANGUAGE plpgsql AS $$
DECLARE
    flight Flights;
BEGIN
    SELECT * INTO flight
    FROM Flights
    WHERE Flightid = flight_id;

    if NOT FOUND THEN
        RETURN FALSE;
    end if;
    RETURN curr_time + INTERVAL '3 hours' < flight.FlightTime AND flight.SellAllowed;
END;
$$;

-- Функция которая возвращает true если в момент времени curr_time можно забронировать
-- билет на рейс flight_id, иначе возвращает false. Билет нельзя забронировать если
-- до вылета рейса осталось менее 3 дней или ReservationAllowed = false.
CREATE OR REPLACE FUNCTION CanReserveFromNow(Flight_id integer, curr_time timestamp)
RETURNS boolean
LANGUAGE plpgsql AS $$
DECLARE
    flight Flights;
BEGIN
    SELECT * INTO flight
    FROM Flights
    WHERE Flightid = Flight_id;

    if NOT FOUND THEN
        RETURN FALSE;
    end if;

    RETURN curr_time + INTERVAL '3 days' < flight.FlightTime AND flight.ReservationAllowed;
END;
$$;

-- Функция которая смотрит на место seat_no на рейсе flight_id и
-- возвращает true если оно свободно, то есть об этом месте нет
-- записи в таблице Tickets, иначе возвращает false.
CREATE OR REPLACE FUNCTION ThisSeatIsEmpty(
    flight_id integer,
    seat_no varchar(4)
)
RETURNS boolean
LANGUAGE plpgsql AS $$
DECLARE
    plane_id integer;
BEGIN

    SELECT PlaneId INTO plane_id
    FROM Flights
    WHERE FlightId = flight_id;

    IF NOT FOUND THEN
        RETURN false;
    END IF;

    IF NOT EXISTS (
       SELECT 1
       FROM Seats
       WHERE SeatNo = seat_no
       AND PlaneId = plane_id
    ) THEN
        RETURN false;
    END IF;

    IF EXISTS (
        SELECT 1
        FROM Tickets
        WHERE FlightId = flight_id
        AND SeatNo = seat_no
    ) THEN
        RETURN false;
    END IF;

    RETURN true;
END;
$$;

-- Функция которая для рейса flight_id смотрит на все билеты,
-- которые забронировали, и удаляет те, время резерва которых уже прошло.
CREATE OR REPLACE PROCEDURE CheckReservationExpiry(flight_id integer)
LANGUAGE plpgsql AS $$
BEGIN
    DELETE FROM Tickets
    WHERE FlightId = flight_id
    AND IsReserved = true
    AND ExpireDate < now();
END;
$$;

CREATE OR REPLACE FUNCTION RegisterUser(user_id integer, user_pass varchar(40))
RETURNS boolean
LANGUAGE plpgsql AS $$
BEGIN
    IF user_id IN (SELECT UserId FROM Users) THEN
        RETURN false;
    END IF;

    INSERT INTO Users VALUES (user_id, crypt(user_pass, gen_salt('bf', 4)));

    RETURN true;
END;
$$;

CREATE OR REPLACE FUNCTION ManageFlight(
    user_id integer,
    user_pass varchar(40),
    flight_id integer,
    sell_allowed boolean,
    reservation_allowed boolean
)
RETURNS void
LANGUAGE plpgsql AS $$
BEGIN
    IF NOT FindUserByPass(user_id, user_pass) THEN
        RETURN;
    end if;

    IF NOT IsAdmin(user_id) THEN
        RETURN;
    END IF;

    UPDATE Flights
    SET SellAllowed = sell_allowed, ReservationAllowed = reservation_allowed
    WHERE FlightId = flight_id;
END;
$$;

CREATE OR REPLACE FUNCTION FreeSeats(flight_id integer)
RETURNS setof varchar(4)
LANGUAGE plpgsql AS $$
DECLARE
    plane_id integer;
BEGIN
    SELECT PlaneId INTO plane_id
    FROM Flights
    WHERE FlightId = flight_id;

    IF NOT FOUND THEN
        RETURN;
    END IF;

    CALL CheckReservationExpiry(flight_id);

    RETURN QUERY
        SELECT S.SeatNo
        FROM Seats AS S
        WHERE S.PlaneId = plane_id

        EXCEPT

        SELECT T.SeatNo
        FROM Tickets AS T
        WHERE T.FlightId = flight_id;
END;
$$;

CREATE OR REPLACE FUNCTION Reserve(
    user_id integer,
    user_pass varchar(40),
    flight_id integer,
    seat_no varchar(4)
)
RETURNS boolean
LANGUAGE plpgsql AS $$
DECLARE
    curr_time timestamp;
BEGIN
    curr_time = now();

    IF NOT FindUserByPass(user_id, user_pass) THEN
        RETURN false;
    END IF;

    CALL CheckReservationExpiry(flight_id);

    IF NOT ThisSeatIsEmpty(flight_id, seat_no) THEN
        RETURN false;
    END IF;

    IF NOT CanReserveFromNow(flight_id, curr_time) THEN
        RETURN false;
    END IF;

    INSERT INTO Tickets VALUES (flight_id, seat_no, user_id, true, curr_time + interval '1 day');

    RETURN true;
END;
$$;

CREATE OR REPLACE FUNCTION ExtendReservation(
    user_id integer,
    user_pass varchar(40),
    flight_id integer,
    seat_no varchar(4)
)
RETURNS boolean
LANGUAGE plpgsql AS $$
DECLARE
    curr_time timestamp;
BEGIN
    curr_time = now();

    IF NOT FindUserByPass(user_id, user_pass) THEN
        RETURN false;
    END IF;

    CALL CheckReservationExpiry(flight_id);

    IF NOT CanReserveFromNow(flight_id, curr_time) THEN
        RETURN false;
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM Tickets
        WHERE UserId = user_id
        AND FlightId = flight_id
        AND SeatNo = seat_no
        AND IsReserved = true
    ) THEN
        RETURN false;
    END IF;

    UPDATE Tickets
    SET ExpireDate = curr_time + interval '1 day'
    WHERE UserId = user_id
    AND FlightId = flight_id
    AND SeatNo = seat_no;

    RETURN true;
END;
$$;


CREATE OR REPLACE FUNCTION BuyFree(
    flight_id integer,
    seat_no varchar(4)
)
RETURNS boolean
LANGUAGE plpgsql AS $$
DECLARE
    curr_time timestamp;
BEGIN
    curr_time = now();

    CALL CheckReservationExpiry(flight_id);

    IF NOT ThisSeatIsEmpty(flight_id, seat_no) THEN
        RETURN false;
    END IF;

    IF NOT CanBuyFromNow(flight_id, curr_time) THEN
        RETURN false;
    END IF;

    INSERT INTO Tickets VALUES (flight_id, seat_no, null, false, null);

    RETURN true;
END;
$$;

CREATE OR REPLACE FUNCTION BuyReserved(
    user_id integer,
    user_pass varchar(40),
    flight_id integer,
    seat_no varchar(4)
)
RETURNS boolean
LANGUAGE plpgsql AS $$
DECLARE
    curr_time timestamp;
BEGIN
    curr_time = now();

    IF NOT FinduserByPass(user_id, user_pass) THEN
        RETURN false;
    END IF;

    CALL CheckReservationExpiry(flight_id);

    IF NOT CanBuyFromNow(flight_id, curr_time) THEN
        RETURN false;
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM Tickets
        WHERE FlightId = flight_id
        AND SeatNo = seat_no
        AND UserId = user_id
        AND IsReserved = true
    ) THEN
        RETURN false;
    END IF;

    UPDATE Tickets
    SET IsReserved = false, ExpireDate = null
    WHERE FlightId = flight_id
    AND SeatNo = seat_no
    AND UserId = user_id;

    RETURN true;
END;
$$;

CREATE OR REPLACE FUNCTION FlightsStatistics(
    user_id integer,
    user_pass varchar(40)
)
RETURNS TABLE(
    FlightId integer,
    CanBuy boolean,
    CanReserve boolean,
    FreeSeatsCount bigint,
    ReservedSeatsCount bigint,
    SoldSeatsCount bigint
)
LANGUAGE plpgsql AS $$
DECLARE
    curr_time timestamp;
BEGIN
    curr_time := now();

    IF NOT FindUserByPass(user_id, user_pass) THEN
        RETURN;
    END IF;


    RETURN QUERY
    SELECT
        F.FlightId,
        CanBuyFromNow(F.FlightId, curr_time) AS CanBuy,
        CanReserveFromNow(F.FlightId, curr_time) AS CanReserve,
        (
            SELECT COUNT(*)
            FROM FreeSeats(F.FlightId)
        ) AS FreeSeatsCount,
        (
            SELECT COUNT(*)
            FROM Tickets AS T
            WHERE T.FlightId = F.FlightId
            AND T.IsReserved = true
        ) AS ReservedSeatsCount,
        (
            SELECT COUNT(*)
            FROM Tickets AS T
            WHERE T.FlightId = F.FlightId
            AND T.IsReserved = false
        ) AS SoldSeatsCount
    FROM Flights AS F;
END;
$$;

CREATE OR REPLACE FUNCTION FlightStat(
    user_id integer,
    user_pass varchar(40),
    flight_id integer
)
RETURNS TABLE(
    FlightId integer,
    CanBuy boolean,
    CanReserve boolean,
    FreeSeatsCount bigint,
    ReservedSeatsCount bigint,
    SoldSeatsCount bigint
)
LANGUAGE plpgsql AS $$
DECLARE
    curr_time timestamp;
BEGIN
    curr_time := now();

    IF NOT FindUserByPass(user_id, user_pass) THEN
        RETURN;
    END IF;


    RETURN QUERY
    SELECT
        F.FlightId,
        CanBuyFromNow(F.FlightId, curr_time) AS CanBuy,
        CanReserveFromNow(F.FlightId, curr_time) AS CanReserve,
        (
            SELECT COUNT(*)
            FROM FreeSeats(F.FlightId)
        ) AS FreeSeatsCount,
        (
            SELECT COUNT(*)
            FROM Tickets AS T
            WHERE T.FlightId = F.FlightId
            AND T.IsReserved = true
        ) AS ReservedSeatsCount,
        (
            SELECT COUNT(*)
            FROM Tickets AS T
            WHERE T.FlightId = F.FlightId
            AND T.IsReserved = false
        ) AS SoldSeatsCount
    FROM Flights AS F
    WHERE F.FlightId = flight_id;
END;
$$;

CREATE OR REPLACE PROCEDURE CompressSeats(
    user_id integer,
    user_pass varchar(40),
    flight_id integer
)
LANGUAGE plpgsql AS $$
DECLARE
    plane_id integer;
    next_seat integer := 1;
    seat_list text[];
    sold_cursor CURSOR FOR
        SELECT SeatNo
        FROM Tickets
        WHERE FlightId = flight_id AND IsReserved = false
        ORDER BY SeatNo
        FOR UPDATE;
    reserved_cursor CURSOR FOR
        SELECT SeatNo
        FROM Tickets
        WHERE FlightId = flight_id AND IsReserved = true
        ORDER BY SeatNo
        FOR UPDATE;
    sold_rec RECORD;
    reserved_rec RECORD;
BEGIN
    IF NOT FindUserByPass(user_id, user_pass) THEN
        RETURN;
    END IF;

    IF NOT IsAdmin(user_id) THEN
        RETURN;
    END IF;

    SELECT PlaneId INTO plane_id
    FROM Flights
    WHERE FlightId = flight_id;

    IF NOT FOUND THEN
        RETURN;
    END IF;

    CALL CheckReservationExpiry(flight_id);


    SELECT array_agg(SeatNo ORDER BY SeatNo) INTO seat_list
    FROM Seats
    WHERE PlaneId = plane_id;

    OPEN sold_cursor;
    LOOP
        FETCH sold_cursor INTO sold_rec;
        EXIT WHEN NOT FOUND;

        UPDATE Tickets
        SET SeatNo = seat_list[next_seat]
        WHERE CURRENT OF sold_cursor;

        next_seat := next_seat + 1;
    END LOOP;
    CLOSE sold_cursor;

    OPEN reserved_cursor;
    LOOP
        FETCH reserved_cursor INTO reserved_rec;
        EXIT WHEN NOT FOUND;

        UPDATE Tickets
        SET SeatNo = seat_list[next_seat]
        WHERE CURRENT OF reserved_cursor;

        next_seat := next_seat + 1;
    END LOOP;
    CLOSE reserved_cursor;
END;
$$;
