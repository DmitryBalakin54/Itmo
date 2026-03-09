-- Таблица рейсов, имеет id(ключ), время вылета,
-- id самолета, и две колонки SellAllowed и ReservationAllowed,
-- которые отвечают за то можно ли купить или забронировать соответственно
-- билет на этот рейс, не зависимо от значений этих колонок, если до вылета
-- осталось 3 часа или меньше, билет купить нельзя, это гарантирует функция
-- CanBuyFromNow и провверки в функциях где производится покупка, аналогично
-- для резервирования места с функцией CanReserveFromNow.
CREATE TABLE Flights (
    FlightId integer PRIMARY KEY,
    FlightTime timestamp NOT NULL,
    PlaneId integer NOT NULL,
    SellAllowed boolean NOT NULL DEFAULT true,
    ReservationAllowed boolean NOT NULL DEFAULT true
);

-- Таблица сидений для каждого самолета с ключом
-- (PlaneId, SeatNo), хранит в себе номер самолета
-- и номер сидения(например 123A).
CREATE TABLE Seats (
    PlaneId integer,
    SeatNo varchar(4) NOT NULL,
    PRIMARY KEY (PlaneId, SeatNo)
);

-- Таблица для хранения пользователей, имеет id(ключ),
-- хэш пароля и булевое значение IsAdmin, которое если true,
-- то пользователь обладает правами администратора, то есть
-- может использовать функции
-- ManageFlight(UserId, Pass, FlightId, SellAllowed, ReservationAllowed)
-- и CompressSeats(UserId, Pass, FlightId).
CREATE TABLE Users (
    UserId integer PRIMARY KEY ,
    PassHash varchar(255) NOT NULL,
    IsAdmin boolean NOT NULL DEFAULT false
);

-- Таблица билетов, имеет ключ (FlightId, SeatNo),
-- содержит в себе номер рейса, номер сидения,
-- пользователя который должен сидеть на этом сидении во
-- время этого рейса (данное значение может быть NULL в силу
-- функции BuyFree(FlightId, SeatNo) в которой не передается пользователь),
-- так же есть булевое значение IsReserved, если true, то значит что место
-- не куплено, а зарезервировано, ExpireDate время истечения резерва данного
-- места, если IsReserved = false, то ExpireDate = null.
CREATE TABLE Tickets (
    FlightId integer NOT NULL,
    SeatNo varchar(4) NOT NULL,
    UserId integer,
    IsReserved boolean NOT NULL DEFAULT false,
    ExpireDate timestamp,
    PRIMARY KEY (FlightId, SeatNo)
);