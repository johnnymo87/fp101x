module CarryoverForRequestDays where

-- type RequestDay = (Float, Float)

-- def apply_carryover_to_request_days(carryover, request_days)
--   return if carryover == 0
--   request_day, *remaining_request_days = request_days
--   return if request_day.nil?
--   wanted_carryover =
--     [request_day.used_accrual - request_day.used_carryover, 0].max
--   if carryover > wanted_carryover
--     request_day.used_carryover += wanted_carryover
--     carryover -= wanted_carryover
--   else
--     request_day.used_carryover = carryover
--     carryover = 0
--   end
--   apply_carryover_to_request_days(carryover, remaining_request_days)
-- end

-- distribute 3 $ [(1,2),(1,1),(1,0),(1,0),(1,0)]]
distribute _ [] = []
distribute 0 as = as
distribute remainder ((ceiling, current):as)
  | remainder > ceiling - current = (ceiling, ceiling) : (distribute (remainder - ceiling) as)
  | otherwise = (ceiling, remainder) : (distribute 0 as)
