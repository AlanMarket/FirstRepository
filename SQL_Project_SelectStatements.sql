use _team26_turo;

select * from finances;


# 1.What type of cars are most commonly rented?
select vehicle_type, round(count(vehicle_type)/(select count(*) from vehicles)*100,2) as 'Percent of Rentals'  from vehicles
group by vehicle_type;

# 2.What is the average duration and cost of rentals by car type?
select vehicle_type, datediff(EndDate, StartDate) as 'Average Duration', datediff(EndDate, StartDate)*PricePerDay as Cost
from rentals as r 
join vehicles as v on v.VID = r.VID
join finances as f 
on r.Order_ID = f.Order_ID
group by vehicle_type order by Cost desc;


# 3.On what day of the week do most rentals start?
select count(*) as 'Total Rentals', dayname(StartDate) as 'Day of the Week'from rentals
group by dayname(StartDate) order by count(*) desc;


# 3.On what day of the week do most rentals end?
select count(*) as 'Total Rentals', dayname(EndDate) as 'Day of the Week'from rentals
group by dayname(EndDate) order by count(*) desc;



# Creating a trigger to enforce price minimum and maximum.
drop trigger if exists priceCap;
delimiter //

create trigger priceCap
before insert on finances
for each row
begin 
	case
    when new.PricePerDay >5000 then set new.PricePerDay = 5000;
    when new.PricePerDay <40 then set new.PricePerDay = 40;
    end case;
end //
delimiter ;


# Creating a function to return the number of rentals for a given host ID
drop function if exists rentalCount;
delimiter //

create function rentalCount(hostID int) returns int(10)
deterministic 
begin

declare rentCount int(10);
set rentCount = (select count(*) from rentals where host_ID= hostid);
return (rentCount);
end //

delimiter ;

#Finding the 10 most prolific hosts

select distinct(host_ID), rentalCount(host_ID) as 'Number of rentals' from rentals
order by rentalCount(host_ID) desc limit 10;


drop function if exists costlyHost;
delimiter //
# Creating a function to find the total misc. cost for a given Host_ID.
create function costlyHost(hostid int) returns decimal(10,2)
deterministic 
begin
	declare costOfHost decimal(10,2);
    set costOfHost = 
    (select sum(miscCost) from finances as f join rentals as r
on f.Order_ID = r.Order_ID
where host_ID= hostid );
return (costOfHost);
end //

delimiter ; 

# Select statement using the two new UDFs
select distinct(hostID), costlyHost(hostID), rentalCount(hostID)
from `host` as h
join rentals as r
on h.hostID = r.host_ID
order by costlyHost(hostID) desc;




# Creating a procedure to return the best performing vehicle class and the number of rentals
# for that class in a given month
drop procedure if exists countByMonth;
delimiter //

create procedure countByMonth (in currentMonth int, out vCount int(5), out vClass varchar(25))
begin

set vCount = 
 (
 select count(Order_ID)from vehicles as v
	join rentals as r
	on v.VID=r.VID
	where month(StartDate) = currentMonth and class = ( select class from vehicles as v
join rentals as r
on v.VID=r.VID
where month(StartDate)=currentMonth
group by class
order by count(Order_ID) desc limit 1)
 );
 set vClass =
 (
  select class from vehicles as v
join rentals as r
on v.VID=r.VID
where month(StartDate)=currentMonth
group by class
order by count(Order_ID) desc
limit 1
 );
 
end //

delimiter ;

call countByMonth(11, @vCount, @vClass);
select @vCount as 'Number of Vehicles', @vClass as 'Vehicle Class';
