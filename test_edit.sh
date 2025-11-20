BOT_TOKEN="8397836728:AAHIzfv9tXGrhprGXNcfIiABrrTyxGux8Q8"
CHAT_ID="495311032"
MESSAGE_ID="2296"

# Send a simple ONE WORD update to prove it works
curl -s -X POST "https://api.telegram.org/bot$BOT_TOKEN/editMessageText" \
     -d chat_id=$CHAT_ID \
     -d message_id=$MESSAGE_ID \
     -d text="TEST_EDIT_SUCCESS"