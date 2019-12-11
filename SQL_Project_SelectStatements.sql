use _team26_turo;

select * from finances;

#What type of cars are most commonly rented?
select vehicle_type, round(count(vehicle_type)/(select count(*) from vehicles)*100,2) as 'Percent of Rentals'  from vehicles
group by vehicle_type;

#What is the average duration and cost of rentals by car type?
select vehicle_type, datediff(EndDate, StartDate) as 'Average Duration', datediff(EndDate, StartDate)*price as Cost from rentals as r 
join vehicles as v on v.VID = r.VID
group by vehicle_type order by Cost desc;


#On what day of the week do most rentals start?
select count(*) as 'Total Rentals', dayname(StartDate) as 'Day of the Week'from rentals
group by dayname(StartDate) order by count(*) desc;


#On what day of the week do most rentals end?
select count(*) as 'Total Rentals', dayname(EndDate) as 'Day of the Week'from rentals
group by dayname(EndDate) order by count(*) desc;

select count(*) from rentals;

drop trigger if exists priceCap;
delimiter //

create trigger priceCap
before insert on vehicles
for each row
begin 
	case
    when new.price >5000 then set new.price = 5000;
    when new.price <40 then set new.price = 40;
    end case;
end //
delimiter ;

drop function if exists rentalCount;
select * from rentals;
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

select distinct(host_ID), rentalCount(host_ID) from rentals
order by rentalCount(host_ID) desc limit 10;

select * from rentals;



drop function if exists costlyHost;

delimiter //

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







