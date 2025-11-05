Bước 1: Chuẩn bị môi trường
Đảm bảo đã cài đặt GHC và cabal. Di chuyển vào thư mục "Caro game haskell" chứa file Server.hs, Client.hs và file .cabal của đồ án.

Bước 2: Biên dịch Ứng dụng
Chạy lệnh sau để cabal tải các thư viện cần thiết (network, stm, async) và biên dịch cả hai file thực thi (server và client):

- cabal build

Bước 3: Khởi động hệ thống (Cần 3 terminal)
Mở ba cửa sổ terminal riêng biệt trong thư mục dự án.
 
Terminal 1 (SERVER): Khởi động server (đang lắng nghe ở cổng 3000) bằng lệnh: 

- cabal run server 

Màn hình sẽ hiển thị "Server is listening on port 3000...". 

Terminal 2 (CLIENT X): Khởi động người chơi X bằng lệnh:

- cabal run client

Màn hình sẽ hiển thị "You are Player X" và đang chờ Player O. 

Terminal 3 (CLIENT O): Khởi động người chơi O bằng lệnh:

- cabal run client

Ván đấu sẽ tự động bắt đầu với lượt của X.